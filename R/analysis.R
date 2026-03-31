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
  profile <- if ("filter_profile" %in% names(df)) {
    unique(df$filter_profile)
  } else {
    NA_character_
  }
  result[,
    c("outcome", "freq", "channel", "n", "filter_profile") := list(
      outcome,
      unique(df$freq),
      unique(df$channel),
      sum(!is.na(df[[predictor]])),
      profile
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
  profile <- if ("filter_profile" %in% names(df)) {
    unique(df$filter_profile)
  } else {
    NA_character_
  }
  result[,
    c("outcome", "freq", "channel", "n", "filter_profile") := list(
      outcome,
      unique(df$freq),
      unique(df$channel),
      sum(complete.cases(df[, .SD, .SDcols = c(predictor, moderator)])),
      profile
    )
  ]
  result
}
