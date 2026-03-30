library(luna)
library(data.table)

build_filtered_edf_command <- function(filtered_dir, base_name, include_artifact_re = FALSE) {
  artifact_re <- if (include_artifact_re) {
    " & RE"
  } else {
    ""
  }
  # QC eeg=C3_M2,C4_M1 &

  sprintf(
    "EPOCH &
    SIGNALS keep=${eeg} &
    ARTIFACTS &
    SIGSTATS &
    CHEP-MASK ep-th=3,3,3 &
    CHEP epoch &
    DUMP-MASK annot=artifacts%s &
    WRITE-ANNOTS file=%s/%s.annots &
    WRITE edf-dir=%s edf=%s",
    artifact_re,
    filtered_dir,
    base_name,
    filtered_dir,
    base_name
  )
}

process_edf <- function(filtered_edf_paths) {
  # filtered_edf_paths may contain both .edf and .annots paths
  # Extract just the .edf file
  filtered_edf_path <- filtered_edf_paths[grepl("\\.edf$", filtered_edf_paths)]
  base_name <- tools::file_path_sans_ext(basename(filtered_edf_path))
  load_edf(filtered_edf_path)

  result <- data.table(
    bach_id = base_name
  )

  leval("MASK ifnot=N2,N3 & RE")
  result$psd <- leval("PSD spectrum epoch")
  result$spindles <- leval(
    "SPINDLES fc=11,15 empirical set-empirical median"
  )
  lrefresh()
  result
}

create_filtered_edf <- function(edf_path) {
  filtered_dir <- file.path(dirname(edf_path), "filtered")
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")
  filtered_name <- paste0(base_name, "_filtered")
  filtered_path <- file.path(filtered_dir, paste0(base_name, "_filtered", ".edf"))
  annot_path <- file.path(filtered_dir, paste0(filtered_name, ".annots"))

  if (!dir.exists(filtered_dir)) {
    dir.create(filtered_dir, recursive = TRUE)
  }

  ledf(edf_path, base_name, xml_path)
  cmd <- build_filtered_edf_command(filtered_dir, filtered_name, include_artifact_re = TRUE)
  print(cmd)
  leval(cmd)
  lrefresh()

  # Return both files so targets tracks them
  c(filtered_path, annot_path)
}

get_filtered_edf_path <- function(edf_path) {
  filtered_dir <- file.path(dirname(edf_path), "filtered")
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  file.path(filtered_dir, paste0(base_name, ".edf"))
}

load_edf <- function(edf_path, xml_path = NULL) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    # Assume XML is named like the original EDF (not the filtered one)
    # For filtered EDFs, the XML is still with the original
    xml_path <- paste0(gsub("_filtered", "", gsub("/filtered/", "/", edf_path)), ".XML")
  }
  ledf(edf_path, base_name, xml_path)
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
    leval("HYPNO epoch"),
    "E",
    cols = c(
      "E",
      "OSTAGE",
      "WASO",
      "PERSISTENT_SLEEP",
      "CLOCK_HOURS",
      "START_SEC"
    )
  )
  stage_epochs <- extract_luna_table(
    leval("STAGE"),
    "E",
    cols = c("E", "CLOCK_TIME", "MINS", "STAGE", "STAGE_N")
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
      stage_epochs,
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
  CH_F <- CH_F[CH %in% channels & EMPTH < 20, ]
  median(CH_F$EMPTH)
}

get_stage_spindles_with_threshold <- function(
  filtered_edf_paths,
  threshold,
  sleep_stage
) {
  # filtered_edf_paths may contain both .edf and .annots paths
  # Extract just the .edf file
  filtered_edf_path <- filtered_edf_paths[grepl("\\.edf$", filtered_edf_paths)]
  base_name <- tools::file_path_sans_ext(basename(filtered_edf_path))
  result <- data.table(
    bach_id = base_name
  )
  load_edf(filtered_edf_path)

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
