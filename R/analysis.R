library(data.table)

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
