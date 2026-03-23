library(luna)
library(data.table)

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

compare_spindles_by_qc <- function(spindle_epochs, epoch_qc, sleep_stage = NULL) {
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

  if (!is.null(sleep_stage) && nrow(qc)) {
    qc <- qc[STAGE == sleep_stage]
  }

  if (!nrow(qc)) {
    return(ensure_dt_cols(data.table(), c(
      "bach_id", "raw_epoch", "CH", "sleep_stage", "E", "F", "N", "START",
      "STOP", "MID", "STAGE", "EMASK", "MASK", "CHEP", "BETA_MASK",
      "DELTA_MASK", "CHEP_CHANNELS_MASKED", "qc_epoch", "noisy_epoch"
    )))
  }

  spindles <- spindles[raw_epoch %in% qc$raw_epoch]
  if (!is.null(sleep_stage) && nrow(spindles)) {
    spindles[, sleep_stage := sleep_stage]
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

build_qc_overview <- function(
  qc_summary,
  raw_spindle_qc_summary,
  edge_epoch_review,
  line_noise_summary
) {
  qc_dt <- ensure_dt_cols(
    copy(as.data.table(qc_summary)),
    c(
      "bach_id",
      "STAGE",
      "CH",
      "n_epochs",
      "n_masked_epochs",
      "pct_masked_epochs",
      "pct_noisy_rows",
      "n_chep_rows",
      "n_artifact_rows"
    )
  )
  spindle_dt <- ensure_dt_cols(
    copy(as.data.table(raw_spindle_qc_summary)),
    c(
      "sleep_stage",
      "CH",
      "F",
      "noisy_epoch",
      "n_epochs",
      "mean_spindles",
      "median_spindles",
      "pct_zero_spindles"
    )
  )
  edge_dt <- ensure_dt_cols(
    copy(as.data.table(edge_epoch_review)),
    c("bach_id", "edge_region", "EMASK", "CHEP_CHANNELS_MASKED")
  )
  line_dt <- ensure_dt_cols(
    copy(as.data.table(line_noise_summary)),
    c("bach_id", "CH", "SPK", "KURT", "SPEC_SLOPE", "SPEC_SLOPE_SD")
  )

  participant_qc <- if (nrow(qc_dt)) {
    qc_dt[, .(
      stages_present = uniqueN(STAGE),
      channels_present = uniqueN(CH),
      epochs_reviewed = sum(n_epochs, na.rm = TRUE),
      masked_epochs = sum(n_masked_epochs, na.rm = TRUE),
      mean_pct_masked = safe_mean_value(pct_masked_epochs),
      max_pct_masked = safe_max_value(pct_masked_epochs),
      mean_pct_noisy = safe_mean_value(pct_noisy_rows)
    ), by = bach_id][order(-max_pct_masked, -mean_pct_noisy, bach_id)]
  } else {
    ensure_dt_cols(data.table(), c(
      "bach_id", "stages_present", "channels_present", "epochs_reviewed",
      "masked_epochs", "mean_pct_masked", "max_pct_masked", "mean_pct_noisy"
    ))
  }

  stage_channel_qc <- if (nrow(qc_dt)) {
    qc_dt[, .(
      participants = uniqueN(bach_id),
      mean_pct_masked = safe_mean_value(pct_masked_epochs),
      median_pct_masked = safe_median_value(pct_masked_epochs),
      max_pct_masked = safe_max_value(pct_masked_epochs),
      mean_pct_noisy = safe_mean_value(pct_noisy_rows)
    ), by = .(STAGE, CH)][order(-mean_pct_masked, STAGE, CH)]
  } else {
    ensure_dt_cols(data.table(), c(
      "STAGE", "CH", "participants", "mean_pct_masked",
      "median_pct_masked", "max_pct_masked", "mean_pct_noisy"
    ))
  }

  spindle_noise_effect <- if (nrow(spindle_dt)) {
    spindle_dt <- spindle_dt[!is.na(noisy_epoch)]
    wide <- dcast(
      spindle_dt,
      sleep_stage + CH + F ~ noisy_epoch,
      value.var = c("n_epochs", "mean_spindles", "median_spindles", "pct_zero_spindles"),
      fill = NA_real_
    )
    setnames(
      wide,
      old = intersect(
        c(
          "n_epochs_FALSE", "n_epochs_TRUE",
          "mean_spindles_FALSE", "mean_spindles_TRUE",
          "median_spindles_FALSE", "median_spindles_TRUE",
          "pct_zero_spindles_FALSE", "pct_zero_spindles_TRUE"
        ),
        names(wide)
      ),
      new = c(
        "clean_epochs", "noisy_epochs",
        "mean_spindles_clean", "mean_spindles_noisy",
        "median_spindles_clean", "median_spindles_noisy",
        "pct_zero_clean", "pct_zero_noisy"
      )[seq_along(intersect(
        c(
          "n_epochs_FALSE", "n_epochs_TRUE",
          "mean_spindles_FALSE", "mean_spindles_TRUE",
          "median_spindles_FALSE", "median_spindles_TRUE",
          "pct_zero_spindles_FALSE", "pct_zero_spindles_TRUE"
        ),
        names(wide)
      ))],
      skip_absent = TRUE
    )
    if (!"mean_spindles_clean" %in% names(wide)) {
      wide[, mean_spindles_clean := NA_real_]
    }
    if (!"mean_spindles_noisy" %in% names(wide)) {
      wide[, mean_spindles_noisy := NA_real_]
    }
    if (!"pct_zero_clean" %in% names(wide)) {
      wide[, pct_zero_clean := NA_real_]
    }
    if (!"pct_zero_noisy" %in% names(wide)) {
      wide[, pct_zero_noisy := NA_real_]
    }
    wide[, `:=`(
      delta_mean_spindles = mean_spindles_noisy - mean_spindles_clean,
      delta_pct_zero = pct_zero_noisy - pct_zero_clean
    )]
    wide[order(-abs(delta_mean_spindles), sleep_stage, CH, F)]
  } else {
    ensure_dt_cols(data.table(), c(
      "sleep_stage", "CH", "F", "clean_epochs", "noisy_epochs",
      "mean_spindles_clean", "mean_spindles_noisy",
      "median_spindles_clean", "median_spindles_noisy",
      "pct_zero_clean", "pct_zero_noisy",
      "delta_mean_spindles", "delta_pct_zero"
    ))
  }

  edge_overview <- if (nrow(edge_dt)) {
    edge_dt[, .(
      reviewed_epochs = .N,
      masked_epochs = sum(coalesce_zero(EMASK) == 1, na.rm = TRUE),
      mean_masked_channels = safe_mean_value(CHEP_CHANNELS_MASKED)
    ), by = .(bach_id, edge_region)][
      , pct_masked := masked_epochs / reviewed_epochs
    ][order(-pct_masked, bach_id, edge_region)]
  } else {
    ensure_dt_cols(data.table(), c(
      "bach_id", "edge_region", "reviewed_epochs",
      "masked_epochs", "mean_masked_channels", "pct_masked"
    ))
  }

  line_noise_overview <- if (nrow(line_dt)) {
    line_dt[
      ,
      .(
        channels_reviewed = uniqueN(CH),
        mean_spk = safe_mean_value(SPK),
        max_spk = safe_max_value(SPK),
        mean_kurt = safe_mean_value(KURT),
        max_kurt = safe_max_value(KURT),
        mean_slope_sd = safe_mean_value(SPEC_SLOPE_SD)
      ),
      by = bach_id
    ][order(-max_spk, -max_kurt, bach_id)]
  } else {
    ensure_dt_cols(data.table(), c(
      "bach_id", "channels_reviewed", "mean_spk", "max_spk",
      "mean_kurt", "max_kurt", "mean_slope_sd"
    ))
  }

  recommendations <- merge(
    participant_qc,
    edge_overview[edge_region == "leading", .(bach_id, leading_edge_masked = pct_masked)],
    by = "bach_id",
    all = TRUE
  )
  recommendations <- merge(
    recommendations,
    edge_overview[edge_region == "trailing", .(bach_id, trailing_edge_masked = pct_masked)],
    by = "bach_id",
    all = TRUE
  )
  recommendations <- merge(
    recommendations,
    line_noise_overview[, .(bach_id, max_spk, max_kurt)],
    by = "bach_id",
    all = TRUE
  )

  if (nrow(recommendations)) {
    recommendations[, `:=`(
      review_edge_trim = coalesce_zero(leading_edge_masked) > 0.25 |
        coalesce_zero(trailing_edge_masked) > 0.25,
      review_line_noise = coalesce_zero(max_spk) > 0 | coalesce_zero(max_kurt) > 10,
      review_noise_bias = coalesce_zero(max_pct_masked) > 0.2
    )]
    setorder(recommendations, -max_pct_masked, -mean_pct_noisy, bach_id)
  }

  list(
    participant_qc = participant_qc,
    stage_channel_qc = stage_channel_qc,
    spindle_noise_effect = spindle_noise_effect,
    edge_overview = edge_overview,
    line_noise_overview = line_noise_overview,
    recommendations = recommendations
  )
}
