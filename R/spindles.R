library(luna)
library(data.table)
library(dplyr)

process_edf <- function(edf_path) {
  filtered_dir <- file.path(dirname(edf_path), "filtered")
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (!dir.exists(filtered_dir)) {
    dir.create(filtered_dir)
  }
  filter_and_load_edf(filtered_dir, edf_path, base_name)

  result <- data.table(
    bach_id = base_name
  )

  ## Compute threshold with N2 and N3 combined
  leval("MASK ifnot=N2,N3 & RE")
  result$psd <- leval("PSD spectrum")
  result$spindles <- leval(
    "SPINDLES fc=11,15 empirical set-empirical median"
  )
  lrefresh()
  result
}

filter_and_load_edf <- function(filtered_dir, edf_path, base_name) {
  xml_path <- paste0(edf_path, ".XML")
  filtered_path <- file.path(filtered_dir, paste0(base_name, ".edf"))

  if (!file.exists(filtered_path)) {
    ledf(edf_path, base_name, xml_path)
    leval(sprintf(
      "EPOCH &
      SIGNALS keep=${eeg} &
      ARTIFACTS &
      SIGSTATS &
      CHEP-MASK ep-th=3,3,3 &
      CHEP epoch &
      DUMP-MASK annot=artifacts &
      WRITE-ANNOTS file=%s/%s.annots &
      WRITE edf-dir=%s",
      filtered_dir,
      base_name,
      filtered_dir
    ))
  }

  ledf(filtered_path, base_name, xml_path)
}

get_empirical_threshold <- function(
  ppt_results,
  channels = c("C3_M2", "C4_M1")
) {
  CH_F <- data.table(ppt_results$spindles[[1]]$CH_F)
  # Don't include maxed out EMPTH in median calc
  CH_F <- CH_F[CH %in% channels & EMPTH < 20, ]
  median(CH_F$EMPTH)
}

get_stage_spindles_with_threshold <- function(
  edf_path,
  threshold,
  sleep_stage
) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  result <- data.table(
    bach_id = base_name
  )
  filtered_dir <- file.path(dirname(edf_path), "filtered")
  filter_and_load_edf(filtered_dir, edf_path, base_name)

  leval(paste0("MASK ifnot=", sleep_stage, " & RE"))
  result$psd <- leval("PSD spectrum")
  result$spindles <- leval(
    paste0(
      "SPINDLES fc=11,15 sig=C3_M2,C4_M1,F3_M2,F4_M1 th=",
      threshold,
      " so f-lwr=0.3 f-upr=4 t-neg-lwr=0.3 t-neg-upr=1.5 t-pos-lwr=0 t-pos-upr=1.0 uV-neg=-40 uV-p2p=75 nreps=100000"
    )
  )
  lrefresh()
  result
}

filter_spindles_so <- function(threshold_results) {
  ensure_cols <- function(x, cols) {
    dt <- as.data.table(x)
    missing <- setdiff(cols, names(dt))
    if (length(missing)) {
      dt[, (missing) := NA]
    }
    dt[, cols, with = FALSE]
  }

  spindle_cols <- c(
    "ID",
    "CH",
    "F",
    "N",
    "AMP",
    "CHIRP",
    "DENS",
    "DUR",
    "COUPL_MAG",
    "COUPL_OVERLAP",
    "COUPL_ANGLE",
    "COUPL_OVERLAP_EMP",
    "COUPL_MAG_EMP",
    "COUPL_OVERLAP_Z",
    "COUPL_MAG_Z",
    "Q"
  )
  so_cols <- c(
    "ID",
    "CH",
    "SO",
    "SO_RATE",
    "SO_NEG_AMP",
    "SO_POS_AMP",
    "SO_P2P",
    "SO_DUR"
  )

  filtered_spindles <- ensure_cols(
    threshold_results$spindles[[1]]$CH_F,
    spindle_cols
  )
  filtered_so <- ensure_cols(threshold_results$spindles[[1]]$CH, so_cols)

  merged_data <- merge(filtered_spindles, filtered_so, by = c("ID", "CH"))
  setDT(merged_data)
  merged_data
}

# Make COUPL_ANGLE NA if COUPL_MAG_EMP > 0.05, ADD flag where true = coupl_mag_emp > 0.05
clean_angle <- function(filtered_results) {
  cleaned_angle <- copy(filtered_results)
  cleaned_angle[, COUPL_ANGLE_EXCLUDED := COUPL_MAG_EMP > 0.05]
  cleaned_angle[COUPL_ANGLE_EXCLUDED == TRUE, COUPL_ANGLE := NA]
}

wrap_angle <- function(ppt_data) {
  ppt_data[
    F == 11,
    COUPL_ANGLE := ifelse(COUPL_ANGLE < 125, COUPL_ANGLE + 360, COUPL_ANGLE)
  ]

  safe_mean <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    } else {
      return(mean(x, na.rm = TRUE))
    }
  }

  result <- ppt_data[,
    .(
      channel = substr(CH[1], 1, 1),
      freq = ifelse(F[1] == 11, "slow", "fast"),
      overlap = safe_mean(COUPL_OVERLAP_Z),
      mag = safe_mean(COUPL_MAG_Z),
      angle = safe_mean(COUPL_ANGLE),
      spindle_count = safe_mean(N),
      spindle_amplitude = safe_mean(AMP),
      spindle_chirp = safe_mean(CHIRP),
      spindle_density = safe_mean(DENS),
      spindle_duration = safe_mean(DUR),
      magnitude_raw = safe_mean(COUPL_MAG),
      overlap_raw = safe_mean(COUPL_OVERLAP),
      SO_count = safe_mean(SO),
      SO_rate = safe_mean(SO_RATE),
      SO_neg_amplitude = safe_mean(SO_NEG_AMP),
      SO_pos_amplitude = safe_mean(SO_POS_AMP),
      SO_peak_to_peak_amplitude = safe_mean(SO_P2P),
      SO_duration = safe_mean(SO_DUR),
      overlap_empirical = safe_mean(COUPL_OVERLAP_EMP),
      mag_empirical = safe_mean(COUPL_MAG_EMP)
    ),
    by = ID
  ]
  result
}

get_model_estimate <- function(df, outcome, predictor) {
  formula_str <- paste0(
    outcome,
    " ~ ",
    predictor,
    " + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso"
  )
  model <- lm(as.formula(formula_str), data = df)
  summary <- summary(model)
  # rn is default rowname when keep.rownames = TRUE
  result <- as.data.table(summary$coefficients, keep.rownames = TRUE)[
    rn == predictor,
  ]
  setnames(result, "rn", "predictor")
  df[, list(ID, freq, channel)]
  result[,
    c("outcome", "freq", "channel", "n") := list(
      outcome,
      unique(df$freq),
      unique(df$channel),
      sum(!is.na(df[[predictor]]))
    )
  ]
}

get_model <- function(df, outcome, predictor, moderator) {
  interaction_term <- paste0(predictor, ":", moderator)

  formula_str <- paste0(
    outcome,
    " ~ ",
    predictor,
    " * ",
    moderator,
    " + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso"
  )

  model <- lm(as.formula(formula_str), data = df)
  model
}

get_interaction_estimates <- function(df, outcome, predictor, moderator) {
  interaction_term <- paste0(predictor, ":", moderator)

  formula_str <- paste0(
    outcome,
    " ~ ",
    predictor,
    " * ",
    moderator,
    " + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso"
  )

  model <- lm(as.formula(formula_str), data = df)
  summary <- summary(model)
  # rn is default rowname when keep.rownames = TRUE
  result <- as.data.table(summary$coefficients, keep.rownames = TRUE)[
    rn == interaction_term,
  ]
  setnames(result, "rn", "interaction_term")
  df[, list(ID, freq, channel)]
  result[,
    c("outcome", "freq", "channel", "n") := list(
      outcome,
      unique(df$freq),
      unique(df$channel),
      sum(complete.cases(df[, .SD, .SDcols = c(predictor, moderator)]))
    )
  ]
  result
}

#################################################
#number of exclued angle values
# NREM_excluded_summary <- cleaned_results_N23[
#   COUPL_ANGLE_EXCLUDED == TRUE,
#   .(excluded_count = .N),
#   by = .(F, CH)
# ]

###############################################
# calculating % of empirical p-values > .05

#N2
#filtering for slow central
# NREM_slow_central <- subset(cleaned_results_N23, F == 11 & CH == "C3_M2")
# NREM_slow_frontal <- subset(cleaned_results_N23, F == 11 & CH == "F3_M2")
#
# NREM_fast_central <- subset(cleaned_results_N23, F == 15 & CH == "C3_M2")
# NREM_fast_frontal <- subset(cleaned_results_N23, F == 15 & CH == "F3_M2")

##########################
#sig_overlap

###############
# sig_overlap <- unique(N2_slow_central$ID[N2_slow_central$COUPL_OVERLAP_EMP < .05])
# sig_overlap_count <- lengths(sig_overlap)
#
# total_overlap <- length(unique(N2_slow_central$ID)) #138
#
# percentage <- (sig_overlap_count / total_overlap) * 100

# # check event count per channel and freq
# summary_table <- filtered_results %>%
#   filter(!is.na(F)) %>%
#   group_by(CH, F) %>%
#   summarise(
#     total_spindles = sum(N, na.rm = TRUE),
#     mean = mean(N, na.rm = TRUE),
#     sd = sd(N, na.rm = TRUE)
#   ) %>%
#   arrange(CH, F)
