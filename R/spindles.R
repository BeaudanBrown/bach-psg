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

safe_mean_value <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_median_value <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  median(x, na.rm = TRUE)
}

safe_max_value <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
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
  leval("EPOCH & SIGNALS keep=${eeg}")

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

  epoch_base <- merge_dt_list(
    list(
      epoch_info,
      hypno_epochs
    ),
    by = "E"
  )

  channel_ids <- unique(na.omit(c(
    artifact_epochs$CH,
    sigstats_epochs$CH,
    chep_epochs$CH,
    artifact_channels$CH,
    sigstats_channels$CH,
    chep_channels$CH
  )))

  if (nrow(epoch_base) && length(channel_ids)) {
    epoch_level <- CJ(
      CH = sort(channel_ids),
      E = sort(unique(epoch_base$E)),
      unique = TRUE
    )
    epoch_level <- merge(epoch_level, epoch_base, by = "E", all.x = TRUE)
    epoch_level <- merge(epoch_level, artifact_epochs, by = c("CH", "E"), all.x = TRUE)
    epoch_level <- merge(epoch_level, sigstats_epochs, by = c("CH", "E"), all.x = TRUE)
    epoch_level <- merge(epoch_level, chep_epochs, by = c("CH", "E"), all.x = TRUE)
  } else {
    epoch_level <- data.table()
  }

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
    # dcast names logical groups as TRUE/FALSE
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
