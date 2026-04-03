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

lookup_keep_channels <- function(
  edf_path,
  channel_keep = PIPELINE_CHANNEL_KEEP_OVERRIDES,
  default_keep_channels = PIPELINE_DEFAULT_KEEP_CHANNELS,
  mandatory_keep_channels = PIPELINE_MANDATORY_KEEP_CHANNELS
) {
  bach_id <- infer_bach_id(edf_path)
  if (!length(channel_keep)) {
    return(unique(c(default_keep_channels, mandatory_keep_channels)))
  }

  matched <- channel_keep[[bach_id]]
  if (is.null(matched)) {
    return(unique(c(default_keep_channels, mandatory_keep_channels)))
  }

  unique(c(unlist(matched, use.names = FALSE), mandatory_keep_channels))
}

create_channel_dropped_edf <- function(edf_path, xml_path = NULL, keep_channels = PIPELINE_DEFAULT_KEEP_CHANNELS) {
  raw_base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    xml_path <- get_raw_xml_path(edf_path)
  }

  channel_drop_dir <- file.path(get_raw_edf_dir(edf_path), "channel_dropped")
  dropped_path <- file.path(channel_drop_dir, paste0(raw_base_name, ".edf"))

  if (!dir.exists(channel_drop_dir)) {
    dir.create(channel_drop_dir, recursive = TRUE)
  }

  if (file.exists(dropped_path)) {
    unlink(dropped_path, force = TRUE)
  }

  ledf(edf_path, raw_base_name, annots = xml_path)
  signal_keep_step <- sprintf("SIGNALS keep=%s", paste(keep_channels, collapse = ","))
  commands <- c(
    signal_keep_step,
    sprintf("WRITE edf-dir=%s edf=%s", dirname(dropped_path), raw_base_name)
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

  if (file.exists(filtered_path)) {
    unlink(filtered_path, force = TRUE)
  }
  if (file.exists(annot_path)) {
    unlink(annot_path, force = TRUE)
  }

  ledf(edf_path, raw_base_name, annots = xml_path)
  commands <- c(
    filter_profile$commands %||% character(),
    sprintf("WRITE-ANNOTS file=%s/%s.annots", filtered_dir, filtered_name),
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
    filtered_annots_path <- file.path(dirname(edf_path), paste0(base_name, ".annots"))
    if (file.exists(filtered_annots_path)) {
      xml_path <- filtered_annots_path
    } else {
      # Assume XML is named like the original EDF (not the filtered one)
      raw_edf_dir <- get_raw_edf_dir(edf_path)
      raw_base_name <- infer_bach_id(edf_path)
      raw_edf_path <- file.path(raw_edf_dir, paste0(raw_base_name, ".edf"))
      xml_path <- get_raw_xml_path(raw_edf_path)
    }
  }
  ledf(edf_path, base_name, annots = xml_path)
}
