library(luna)
library(data.table)

get_raw_input_summary <- function(edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- get_raw_xml_path(edf_path)

  ledf(edf_path, base_name, annots = xml_path)
  leval("EPOCH")

  initial_mask <- extract_luna_table(
    leval("DUMP-MASK"),
    "E",
    cols = c("E", "EMASK")
  )

  total_epochs <- nrow(initial_mask)
  masked_epochs <- if (nrow(initial_mask)) {
    sum(as.logical(initial_mask$EMASK), na.rm = TRUE)
  } else {
    0L
  }

  leval("MASK clear")
  stage_epochs <- extract_luna_table(
    leval("STAGE"),
    "E",
    cols = c("E", "STAGE")
  )
  lrefresh()

  stage_summary <- if (nrow(stage_epochs)) {
    dcast(
      stage_epochs[, .N, by = STAGE],
      . ~ STAGE,
      value.var = "N",
      fill = 0
    )
  } else {
    data.table(. = 1L)
  }

  summary <- data.table(
    bach_id = base_name,
    raw_edf_path = edf_path,
    raw_xml_path = xml_path,
    total_epochs = total_epochs,
    inherited_masked_epochs = masked_epochs,
    inherited_unmasked_epochs = total_epochs - masked_epochs
  )

  if (nrow(stage_summary)) {
    stage_summary[, . := NULL]
    summary <- cbind(summary, stage_summary)
  }

  summary
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
