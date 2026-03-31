library(luna)

get_raw_xml_path <- function(edf_path) {
  candidates <- c(
    paste0(edf_path, ".XML"),
    file.path(
      dirname(edf_path),
      paste0(tools::file_path_sans_ext(basename(edf_path)), ".XML")
    )
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(existing[[1]])
  }
  candidates[[1]]
}

lookup_channel_exclusions <- function(edf_path, channel_exclusions = PIPELINE_CHANNEL_EXCLUSIONS) {
  bach_id <- infer_bach_id(edf_path)
  if (!nrow(channel_exclusions)) {
    return(character())
  }

  matched <- channel_exclusions[channel_exclusions$bach_id == bach_id]
  if (!nrow(matched)) {
    return(character())
  }

  unique(unlist(matched$drop_channels, use.names = FALSE))
}

create_channel_dropped_edf <- function(edf_path, xml_path = NULL, drop_channels = character()) {
  raw_base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    xml_path <- get_raw_xml_path(edf_path)
  }

  channel_drop_dir <- file.path(get_raw_edf_dir(edf_path), "channel_dropped")
  dropped_path <- file.path(channel_drop_dir, paste0(raw_base_name, ".edf"))

  if (!dir.exists(channel_drop_dir)) {
    dir.create(channel_drop_dir, recursive = TRUE)
  }

  ledf(edf_path, raw_base_name, annots = xml_path)
  signal_drop_step <- if (length(drop_channels)) {
    sprintf("SIGNALS drop=%s", paste(drop_channels, collapse = ","))
  }
  commands <- c(
    "SIGNALS keep=${eeg}",
    signal_drop_step,
    sprintf("WRITE edf-dir=%s edf=%s", dirname(dropped_path), basename(dropped_path))
  )
  cmd <- paste(commands[nzchar(commands)], collapse = " &\n    ")
  print(cmd)
  leval(cmd)
  lrefresh()

  dropped_path
}

create_filtered_edf <- function(edf_path, xml_path = NULL, filter_profile_name = "base", filter_profile = NULL) {
  filtered_dir <- file.path(get_raw_edf_dir(edf_path), "filtered", filter_profile_name)
  raw_base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    xml_path <- get_raw_xml_path(edf_path)
  }
  filtered_name <- paste0(raw_base_name, "_filtered")
  filtered_path <- file.path(filtered_dir, paste0(raw_base_name, "_filtered", ".edf"))
  annot_path <- file.path(filtered_dir, paste0(filtered_name, ".annots"))

  if (!dir.exists(filtered_dir)) {
    dir.create(filtered_dir, recursive = TRUE)
  }

  ledf(edf_path, raw_base_name, annots = xml_path)
  filter_commands <- filter_profile$filter_commands %||% character()
  commands <- c(
    "EPOCH",
    "SUPPRESS-ECG ecg=ECG",
    "SIGNALS keep=${eeg}",
    "MASK clear",
    "EDGER sig=${eeg} epoch mask",
    filter_commands,
    "ARTIFACTS",
    "SIGSTATS",
    PIPELINE_DEFAULT_QC_COMMANDS,
    "CHEP epoch",
    "DUMP-MASK annot=artifacts",
    "QC eeg=C3_M2,C4_M1",
    sprintf("WRITE-ANNOTS file=%s/%s.annots annot=artifacts", filtered_dir, filtered_name),
    sprintf("WRITE edf-dir=%s edf=%s", filtered_dir, filtered_name)
  )
  cmd <- paste(commands[nzchar(commands)], collapse = " &\n    ")
  print(cmd)
  leval(cmd)
  lrefresh()

  # Return both files so targets tracks them
  c(filtered_path, annot_path)
}

load_edf <- function(edf_path, xml_path = NULL) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    # Assume XML is named like the original EDF (not the filtered one)
    # For filtered EDFs, the XML is still with the original
    raw_edf_dir <- get_raw_edf_dir(edf_path)
    raw_base_name <- infer_bach_id(edf_path)
    raw_edf_path <- file.path(raw_edf_dir, paste0(raw_base_name, ".edf"))
    xml_path <- get_raw_xml_path(raw_edf_path)
    filtered_annots_path <- file.path(dirname(edf_path), paste0(base_name, ".annots"))
    if (file.exists(filtered_annots_path)) {
      xml_path <- c(xml_path, filtered_annots_path)
    }
  }
  ledf(edf_path, base_name, annots = xml_path)
}
