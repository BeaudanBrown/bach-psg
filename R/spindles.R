library(luna)
library(data.table)

extract_luna_table <- function(result, table_name, cols = NULL) {
  if (length(result) == 0 || is.null(result[[1]]) || is.null(result[[1]][[table_name]])) {
    dt <- data.table()
  } else {
    dt <- as.data.table(result[[1]][[table_name]])
  }

  if (!is.null(cols)) {
    missing <- setdiff(cols, names(dt))
    if (length(missing)) {
      dt[, (missing) := NA]
    }
    dt <- dt[, cols, with = FALSE]
  }

  dt
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

extract_first_available_table <- function(result, table_names, cols = NULL) {
  result_names <- names(result[[1]] %||% list())
  for (table_name in table_names) {
    dt <- extract_luna_table(result, table_name, cols = cols)
    if (nrow(dt) > 0 || table_name %in% result_names) {
      return(dt)
    }
  }

  data.table()
}

merge_dt_list <- function(dt_list, by) {
  non_empty <- Filter(function(x) nrow(x) > 0, dt_list)
  if (!length(non_empty)) {
    return(data.table())
  }

  Reduce(function(x, y) merge(x, y, by = by, all = TRUE), non_empty)
}

ensure_dt_cols <- function(x, cols) {
  dt <- as.data.table(x)
  missing <- setdiff(cols, names(dt))
  if (length(missing)) {
    dt[, (missing) := NA]
  }
  dt[, cols, with = FALSE]
}

coalesce_zero <- function(x) {
  y <- copy(x)
  y[is.na(y)] <- 0
  y
}

collect_tables <- function(x) {
  if (is.null(x)) {
    return(list())
  }

  if (is.data.table(x) || is.data.frame(x)) {
    return(list(as.data.table(x)))
  }

  if (is.list(x)) {
    return(unlist(lapply(x, collect_tables), recursive = FALSE))
  }

  list()
}

process_edf <- function(edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  load_filtered_edf(edf_path)

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

ensure_filtered_edf <- function(edf_path) {
  filtered_dir <- file.path(dirname(edf_path), "filtered")
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")
  filtered_path <- file.path(filtered_dir, paste0(base_name, ".edf"))

  if (!dir.exists(filtered_dir)) {
    dir.create(filtered_dir)
  }

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

  filtered_path
}

load_filtered_edf <- function(edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")
  filtered_path <- ensure_filtered_edf(edf_path)

  ledf(filtered_path, base_name, xml_path)
}

get_raw_qc_data <- function(edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")

  ledf(edf_path, base_name, xml_path)
  leval("EPOCH")

  epoch_info <- extract_luna_table(
    leval("EPOCH verbose"),
    "E",
    cols = c("E", "E1", "HMS", "START", "STOP", "MID", "INTERVAL", "TP")
  )
  hypno_epochs <- extract_luna_table(
    leval("HYPNO"),
    "E",
    cols = c(
      "E",
      "CLOCK_TIME",
      "CLOCK_HOURS",
      "START_SEC",
      "STAGE",
      "STAGE_N",
      "OSTAGE",
      "WASO",
      "PERSISTENT_SLEEP"
    )
  )
  artifacts <- leval("ARTIFACTS verbose")
  artifact_epochs <- extract_luna_table(
    artifacts,
    "CH_E",
    cols = c(
      "CH",
      "E",
      "BETA",
      "BETA_AVG",
      "BETA_FAC",
      "BETA_MASK",
      "DELTA",
      "DELTA_AVG",
      "DELTA_FAC",
      "DELTA_MASK",
      "MASK"
    )
  )
  artifact_channels <- extract_luna_table(
    artifacts,
    "CH",
    cols = c("CH", "ALTERED_EPOCHS", "FLAGGED_EPOCHS", "TOTAL_EPOCHS")
  )
  if (nrow(artifact_channels)) {
    setnames(
      artifact_channels,
      c("ALTERED_EPOCHS", "FLAGGED_EPOCHS", "TOTAL_EPOCHS"),
      c(
        "artifact_altered_epochs",
        "artifact_flagged_epochs",
        "artifact_total_epochs"
      ),
      skip_absent = TRUE
    )
  }
  sigstats <- leval("SIGSTATS epoch")
  sigstats_epochs <- extract_luna_table(
    sigstats,
    "CH_E",
    cols = c("CH", "E", "H1", "H2", "H3")
  )
  sigstats_channels <- extract_luna_table(
    sigstats,
    "CH",
    cols = c(
      "CH",
      "ALTERED_EPOCHS",
      "CNT_ACT",
      "CNT_CLP",
      "CNT_CMP",
      "CNT_MOB",
      "CNT_RMS",
      "FLAGGED_EPOCHS",
      "TOTAL_EPOCHS",
      "P_H1",
      "P_H2",
      "P_H3",
      "P_OUT"
    )
  )
  if (nrow(sigstats_channels)) {
    setnames(
      sigstats_channels,
      c("ALTERED_EPOCHS", "FLAGGED_EPOCHS", "TOTAL_EPOCHS"),
      c(
        "sigstats_altered_epochs",
        "sigstats_flagged_epochs",
        "sigstats_total_epochs"
      ),
      skip_absent = TRUE
    )
  }

  leval("CHEP-MASK ep-th=3,3,3")
  chep_dump <- leval("CHEP dump")
  chep_epochs <- extract_luna_table(
    chep_dump,
    "CH_E",
    cols = c("CH", "E", "CHEP")
  )
  chep_epoch_summary <- extract_luna_table(
    chep_dump,
    "E",
    cols = c("E", "CHEP")
  )
  chep_channels <- extract_luna_table(
    chep_dump,
    "CH",
    cols = c("CH", "CHEP")
  )

  leval("CHEP epoch")
  epoch_mask <- extract_luna_table(
    leval("DUMP-MASK"),
    "E",
    cols = c("E", "EMASK")
  )

  lrefresh()

  epoch_level <- merge_dt_list(
    list(
      artifact_epochs,
      sigstats_epochs,
      chep_epochs
    ),
    by = c("CH", "E")
  )

  epoch_level <- merge_dt_list(
    list(
      epoch_level,
      epoch_info,
      hypno_epochs
    ),
    by = "E"
  )

  if (nrow(chep_epoch_summary)) {
    setnames(chep_epoch_summary, "CHEP", "CHEP_CHANNELS_MASKED")
    epoch_level <- merge(epoch_level, chep_epoch_summary, by = "E", all = TRUE)
  }

  if (nrow(epoch_mask)) {
    epoch_level <- merge(epoch_level, epoch_mask, by = "E", all = TRUE)
  }

  if (nrow(epoch_level)) {
    epoch_level[, bach_id := base_name]
    setcolorder(epoch_level, c("bach_id", "E", "CH"))
  }

  channel_level <- merge_dt_list(
    list(
      artifact_channels,
      sigstats_channels,
      chep_channels
    ),
    by = "CH"
  )

  if (nrow(channel_level)) {
    channel_level[, bach_id := base_name]
    setcolorder(channel_level, c("bach_id", "CH"))
  }

  list(
    epoch_qc = epoch_level,
    channel_qc = channel_level
  )
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
  load_filtered_edf(edf_path)

  leval(paste0("MASK ifnot=", sleep_stage, " & RE"))
  result$psd <- leval("PSD spectrum")
  result$epoch_map <- leval("EPOCH verbose")
  result$spindles <- leval(
    paste0(
      "SPINDLES fc=11,15 sig=C3_M2,C4_M1,F3_M2,F4_M1 th=",
      threshold,
      " epoch so f-lwr=0.3 f-upr=4 t-neg-lwr=0.3 t-neg-upr=1.5 t-pos-lwr=0 t-pos-upr=1.0 uV-neg=-40 uV-p2p=75 nreps=100000"
    )
  )
  lrefresh()
  result
}

get_raw_stage_spindles_with_threshold <- function(
  edf_path,
  threshold,
  sleep_stage
) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")

  ledf(edf_path, base_name, xml_path)
  leval("EPOCH & SIGNALS keep=${eeg}")
  result <- data.table(
    bach_id = base_name
  )

  leval(paste0("MASK ifnot=", sleep_stage))
  result$epoch_map <- leval("EPOCH verbose")
  result$spindles <- leval(
    paste0(
      "SPINDLES fc=11,15 sig=C3_M2,C4_M1,F3_M2,F4_M1 th=",
      threshold,
      " epoch so f-lwr=0.3 f-upr=4 t-neg-lwr=0.3 t-neg-upr=1.5 t-pos-lwr=0 t-pos-upr=1.0 uV-neg=-40 uV-p2p=75 nreps=100000"
    )
  )
  lrefresh()
  result
}

extract_spindle_epoch_counts <- function(stage_threshold_results, sleep_stage) {
  epoch_map <- extract_luna_table(
    stage_threshold_results$epoch_map,
    "E",
    cols = c("E", "E1", "START", "STOP", "MID")
  )
  spindle_epochs <- extract_luna_table(
    stage_threshold_results$spindles,
    "CH_E_F",
    cols = c("CH", "E", "F", "N")
  )

  if (nrow(spindle_epochs) && nrow(epoch_map)) {
    spindle_epochs <- merge(spindle_epochs, epoch_map, by = "E", all.x = TRUE)
  }

  if (nrow(spindle_epochs)) {
    setnames(spindle_epochs, "E1", "raw_epoch", skip_absent = TRUE)
    if (!"raw_epoch" %in% names(spindle_epochs)) {
      spindle_epochs[, raw_epoch := E]
    }
    spindle_epochs[is.na(raw_epoch), raw_epoch := E]
    spindle_epochs[, `:=`(
      bach_id = stage_threshold_results$bach_id[[1]],
      sleep_stage = sleep_stage
    )]
    setcolorder(spindle_epochs, c("bach_id", "sleep_stage", "CH", "E", "raw_epoch", "F", "N"))
  }

  ensure_dt_cols(
    spindle_epochs,
    c(
      "bach_id",
      "sleep_stage",
      "CH",
      "E",
      "raw_epoch",
      "F",
      "N",
      "START",
      "STOP",
      "MID"
    )
  )
}

summarize_epoch_qc <- function(epoch_qc) {
  dt <- ensure_dt_cols(
    copy(as.data.table(epoch_qc)),
    c("bach_id", "E", "CH", "STAGE", "EMASK", "MASK", "CHEP", "BETA_MASK", "DELTA_MASK")
  )
  if (!nrow(dt)) {
    return(ensure_dt_cols(data.table(), c(
      "bach_id", "STAGE", "CH", "n_rows", "n_epochs", "n_masked_epochs",
      "pct_masked_epochs", "n_noisy_rows", "pct_noisy_rows", "n_chep_rows", "n_artifact_rows"
    )))
  }

  dt[, noisy_epoch := fifelse(
    coalesce_zero(EMASK) == 1 |
      coalesce_zero(MASK) == 1 |
      coalesce_zero(CHEP) == 1 |
      coalesce_zero(BETA_MASK) == 1 |
      coalesce_zero(DELTA_MASK) == 1,
    TRUE,
    FALSE
  )]

  dt[, .(
    n_rows = .N,
    n_epochs = uniqueN(E),
    n_masked_epochs = uniqueN(E[coalesce_zero(EMASK) == 1]),
    pct_masked_epochs = uniqueN(E[coalesce_zero(EMASK) == 1]) / uniqueN(E),
    n_noisy_rows = sum(noisy_epoch, na.rm = TRUE),
    pct_noisy_rows = mean(noisy_epoch, na.rm = TRUE),
    n_chep_rows = sum(coalesce_zero(CHEP) == 1, na.rm = TRUE),
    n_artifact_rows = sum(coalesce_zero(MASK) == 1, na.rm = TRUE)
  ), by = .(bach_id, STAGE, CH)]
}

get_edge_epoch_review <- function(epoch_qc, n_edge_epochs = 10) {
  dt <- ensure_dt_cols(
    copy(as.data.table(epoch_qc)),
    c("bach_id", "E", "STAGE", "EMASK", "CHEP_CHANNELS_MASKED", "START", "STOP", "HMS")
  )
  if (!nrow(dt)) {
    return(ensure_dt_cols(data.table(), c(
      "bach_id", "E", "STAGE", "EMASK", "CHEP_CHANNELS_MASKED", "START",
      "STOP", "HMS", "epoch_order", "total_epochs", "edge_region"
    )))
  }

  epoch_dt <- unique(
    dt[, .(
      bach_id,
      E,
      STAGE,
      EMASK,
      CHEP_CHANNELS_MASKED,
      START,
      STOP,
      HMS
    )]
  )

  setorder(epoch_dt, bach_id, E)
  epoch_dt[, epoch_order := seq_len(.N), by = bach_id]
  epoch_dt[, total_epochs := .N, by = bach_id]
  epoch_dt[
    epoch_order <= n_edge_epochs | epoch_order > (total_epochs - n_edge_epochs),
    edge_region := fifelse(epoch_order <= n_edge_epochs, "leading", "trailing")
  ]
}

get_line_noise_review <- function(edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")

  ledf(edf_path, base_name, xml_path)
  leval("EPOCH & SIGNALS keep=${eeg}")
  psd <- leval("PSD spectrum peaks max=60")
  lrefresh()

  summary_dt <- extract_luna_table(
    psd,
    "CH",
    cols = c("CH", "KURT", "NE", "SPEC_SLOPE", "SPEC_SLOPE_MD", "SPEC_SLOPE_MN", "SPEC_SLOPE_SD", "SPK")
  )
  band_dt <- extract_first_available_table(
    psd,
    table_names = c("B_CH", "CH_F"),
    cols = NULL
  )

  if (nrow(summary_dt)) {
    summary_dt[, bach_id := base_name]
    setcolorder(summary_dt, c("bach_id", "CH"))
  }

  if (nrow(band_dt)) {
    band_dt[, bach_id := base_name]
  }

  list(
    summary = summary_dt,
    spectra = band_dt
  )
}

compare_spindles_by_qc <- function(spindle_epochs, epoch_qc) {
  spindles <- ensure_dt_cols(
    copy(as.data.table(spindle_epochs)),
    c("bach_id", "sleep_stage", "CH", "E", "raw_epoch", "F", "N", "START", "STOP", "MID")
  )
  if (!nrow(spindles)) {
    return(ensure_dt_cols(data.table(), c(
      "bach_id", "raw_epoch", "CH", "sleep_stage", "E", "F", "N", "START",
      "STOP", "MID", "STAGE", "EMASK", "MASK", "CHEP", "BETA_MASK",
      "DELTA_MASK", "CHEP_CHANNELS_MASKED", "noisy_epoch"
    )))
  }

  qc <- unique(
    ensure_dt_cols(
      as.data.table(epoch_qc),
      c(
        "bach_id",
        "E",
        "CH",
        "STAGE",
        "EMASK",
        "MASK",
        "CHEP",
        "BETA_MASK",
        "DELTA_MASK",
        "CHEP_CHANNELS_MASKED"
      )
    )[, `:=`(raw_epoch = E, qc_epoch = E)][, E := NULL][]
  )

  if (!nrow(qc)) {
    out <- copy(spindles)
    out[, `:=`(
      STAGE = NA_character_,
      EMASK = NA,
      MASK = NA,
      CHEP = NA,
      BETA_MASK = NA,
      DELTA_MASK = NA,
      CHEP_CHANNELS_MASKED = NA,
      noisy_epoch = NA
    )]
    return(out)
  }

  out <- merge(spindles, qc, by = c("bach_id", "raw_epoch", "CH"), all.x = TRUE)
  out[, noisy_epoch := fifelse(
    coalesce_zero(EMASK) == 1 |
      coalesce_zero(MASK) == 1 |
      coalesce_zero(CHEP) == 1 |
      coalesce_zero(BETA_MASK) == 1 |
      coalesce_zero(DELTA_MASK) == 1,
    TRUE,
    FALSE
  )]
  out
}

summarize_spindle_qc <- function(spindle_qc) {
  dt <- ensure_dt_cols(
    copy(as.data.table(spindle_qc)),
    c("sleep_stage", "CH", "F", "N", "noisy_epoch")
  )
  if (!nrow(dt)) {
    return(ensure_dt_cols(data.table(), c(
      "sleep_stage", "CH", "F", "noisy_epoch", "n_epochs",
      "mean_spindles", "median_spindles", "pct_zero_spindles"
    )))
  }

  dt[, .(
    n_epochs = .N,
    mean_spindles = mean(N, na.rm = TRUE),
    median_spindles = median(N, na.rm = TRUE),
    pct_zero_spindles = mean(N == 0, na.rm = TRUE)
  ), by = .(sleep_stage, CH, F, noisy_epoch)]
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
