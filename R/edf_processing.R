library(luna)
library(data.table)

DEFAULT_FILTER_COMMANDS <- c(
  "CHEP-MASK ep-th=3,3,3 max=200,0.05 clipped=0.05 flat=0.05"
)

infer_filter_profile <- function(edf_path) {
  edf_dir <- dirname(edf_path)
  if (grepl("/filtered/", edf_dir, perl = TRUE)) {
    profile <- basename(edf_dir)
    return(ifelse(profile == "filtered", "base", profile))
  }
  "base"
}

infer_bach_id <- function(edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  sub("_filtered$", "", base_name)
}

build_filtered_edf_command <- function(
  filtered_dir,
  base_name,
  filter_profile = NULL
) {
  profile <- list(
    filter_commands = DEFAULT_FILTER_COMMANDS
  )

  if (!is.null(filter_profile)) {
    if (!is.list(filter_profile)) {
      stop("filter_profile must be a list")
    }

    custom_commands <- filter_profile$filter_commands
    if (!is.null(custom_commands)) {
      if (!is.character(custom_commands)) {
        stop("filter_profile$filter_commands must be a character vector")
      }
      custom_commands <- custom_commands[
        !is.na(custom_commands) & nzchar(custom_commands)
      ]
      if (length(custom_commands)) {
        profile$filter_commands <- c(profile$filter_commands, custom_commands)
      }
    }

    unexpected <- setdiff(names(filter_profile), "filter_commands")
    if (length(unexpected)) {
      stop(
        sprintf(
          "Unexpected filter_profile entries: %s",
          paste(unexpected, collapse = ", ")
        )
      )
    }
  }

  artifact_re <- " & RE"

  filter_commands <- vapply(
    profile$filter_commands,
    as.character,
    character(1),
    USE.NAMES = FALSE
  )
  if (!length(filter_commands)) {
    filter_commands <- DEFAULT_FILTER_COMMANDS
  }

  # Keep filters first, then keep annotation/qc export unchanged.
  filter_chain <- paste(filter_commands, collapse = " & ")

  sprintf(
    "EPOCH &
    SUPPRESS-ECG ecg=ECG &
    SIGNALS keep=${eeg} &
    EDGER sig=${eeg} epoch mask &
    ARTIFACTS &
    SIGSTATS &
    %s &
    CHEP epoch &
    DUMP-MASK annot=artifacts%s &
    QC eeg=C3_M2,C4_M1 &
    WRITE-ANNOTS file=%s/%s.annots &
    WRITE edf-dir=%s edf=%s",
    filter_chain,
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
  base_name <- infer_bach_id(filtered_edf_path)
  filter_profile <- infer_filter_profile(filtered_edf_path)
  load_edf(filtered_edf_path)

  result <- data.table(
    bach_id = base_name,
    filter_profile = filter_profile
  )

  leval("MASK ifnot=N2,N3 & RE")
  result$psd <- leval("PSD spectrum epoch")
  result$spindles <- leval(
    "SPINDLES fc=11,15 empirical set-empirical median"
  )
  lrefresh()
  result
}

create_filtered_edf <- function(edf_path, filter_profile_name = "base", filter_profile = NULL) {
  filtered_dir <- file.path(dirname(edf_path), "filtered", filter_profile_name)
  raw_base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")
  filtered_name <- paste0(raw_base_name, "_filtered")
  filtered_path <- file.path(filtered_dir, paste0(raw_base_name, "_filtered", ".edf"))
  annot_path <- file.path(filtered_dir, paste0(filtered_name, ".annots"))

  if (!dir.exists(filtered_dir)) {
    dir.create(filtered_dir, recursive = TRUE)
  }

  ledf(edf_path, raw_base_name, xml_path)
  if (!is.null(filter_profile) && !is.list(filter_profile)) {
    stop("filter_profile must be a list")
  }
  cmd <- build_filtered_edf_command(
    filtered_dir = filtered_dir,
    base_name = filtered_name,
    filter_profile = filter_profile
  )
  print(cmd)
  leval(cmd)
  lrefresh()

  # Return both files so targets tracks them
  c(filtered_path, annot_path)
}

get_filtered_edf_path <- function(edf_path, filter_profile_name = "base") {
  filtered_dir <- file.path(dirname(edf_path), "filtered", filter_profile_name)
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  file.path(filtered_dir, paste0(base_name, "_filtered", ".edf"))
}

load_edf <- function(edf_path, xml_path = NULL) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    # Assume XML is named like the original EDF (not the filtered one)
    # For filtered EDFs, the XML is still with the original
    raw_dir <- if (grepl("/filtered", dirname(edf_path), perl = TRUE)) {
      sub("/filtered(?:/[^/]+)?$", "", dirname(edf_path), perl = TRUE)
    } else {
      dirname(edf_path)
    }
    raw_base_name <- infer_bach_id(edf_path)
    xml_path <- file.path(raw_dir, paste0(raw_base_name, ".XML"))
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
