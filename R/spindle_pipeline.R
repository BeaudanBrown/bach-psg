# Spindle+SO post-processing for thresholded detections
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
  if (nrow(filtered_spindles)) {
    filtered_spindles[, ID := threshold_results$bach_id[[1]]]
  }
  if (nrow(filtered_so)) {
    filtered_so[, ID := threshold_results$bach_id[[1]]]
  }
  profile <- if ("filter_profile" %in% names(threshold_results)) {
    threshold_results$filter_profile
  } else {
    NA_character_
  }
  if (nrow(filtered_spindles)) {
    filtered_spindles[, filter_profile := profile[[1]]]
  }
  if (nrow(filtered_so)) {
    filtered_so[, filter_profile := profile[[1]]]
  }

  merged_data <- merge(filtered_spindles, filtered_so, by = c("ID", "CH"))
  if ("filter_profile" %in% names(merged_data)) {
    setcolorder(
      merged_data,
      c("ID", "filter_profile", setdiff(names(merged_data), c("ID", "filter_profile")))
    )
  }
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
    by = .(ID, filter_profile)
  ]
  result
}

get_empirical_threshold <- function(
  ppt_results,
  channels = NULL
) {
  if (is.null(channels)) {
    channels <- c("C3_M2", "C4_M1")
  }
  CH_F <- data.table(ppt_results$spindles[[1]]$CH_F)
  CH_F <- CH_F[CH %in% channels & EMPTH < 20, ]
  median(CH_F$EMPTH)
}

get_empirical_threshold_by_profile <- function(
  edf_results,
  channels = NULL
) {
  if (is.null(channels)) {
    channels <- c("C3_M2", "C4_M1")
  }
  results_by_profile <- split(
    edf_results,
    vapply(
      edf_results,
      function(x) {
        if ("filter_profile" %in% names(x)) {
          x$filter_profile
        } else {
          "base"
        }
      },
      "",
      USE.NAMES = TRUE
    )
  )
  vapply(
    results_by_profile,
          get_empirical_threshold,
          numeric(1),
          channels = channels
  )
}

get_stage_spindles_with_threshold <- function(
  filtered_edf_paths,
  threshold,
  sleep_stage
) {
  # filtered_edf_paths may contain both .edf and .annots paths
  # Extract just the .edf file
  filtered_edf_path <- filtered_edf_paths[grepl("\\.edf$", filtered_edf_paths)]
  base_name <- infer_bach_id(filtered_edf_path)
  filter_profile <- infer_filter_profile(filtered_edf_path)
  result <- data.table(
    bach_id = base_name,
    filter_profile = filter_profile
  )
  load_edf(filtered_edf_path)

  leval(paste0("MASK ifnot=", sleep_stage, " & RE"))
  result$psd <- leval("PSD spectrum")
  result$epoch_map <- leval("EPOCH verbose")
  result$spindles <- leval(build_spindle_command(threshold))
  lrefresh()
  result
}

get_raw_stage_spindles_with_threshold <- function(
  edf_path,
  xml_path = NULL,
  threshold,
  sleep_stage
) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    xml_path <- get_raw_xml_path(edf_path)
  }

  ledf(edf_path, base_name, annots = xml_path)
  leval("EPOCH & SIGNALS keep=${eeg}")
  result <- data.table(
    bach_id = base_name
  )

  result$epoch_map <- leval("EPOCH verbose")
  result$spindles <- leval(build_spindle_command(threshold))
  lrefresh()
  result
}

build_spindle_command <- function(threshold, channels = NULL) {
  if (is.null(channels)) {
    channels <- c("C3_M2", "C4_M1", "F3_M2", "F4_M1")
  }
  sig_channels <- paste(channels, collapse = ",")
  paste0(
    "SPINDLES fc=11,15 sig=",
    sig_channels,
    " th=",
    threshold,
    " epoch so f-lwr=0.3 f-upr=4 t-neg-lwr=0.3 t-neg-upr=1.5 t-pos-lwr=0 t-pos-upr=1.0 uV-neg=-40 uV-p2p=75 nreps=100000"
  )
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
      filter_profile = stage_threshold_results$filter_profile[[1]],
      sleep_stage = sleep_stage
    )]
    setcolorder(
      spindle_epochs,
      c(
        "bach_id",
        "filter_profile",
        "sleep_stage",
        "CH",
        "E",
        "raw_epoch",
        "F",
        "N"
      )
    )
  }

  ensure_dt_cols(
    spindle_epochs,
    c(
      "bach_id",
      "filter_profile",
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
