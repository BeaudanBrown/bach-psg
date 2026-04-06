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

is_excluded_keep_override <- function(override_value) {
  is.null(override_value)
}

has_keep_override <- function(bach_id, channel_keep = PIPELINE_CHANNEL_KEEP_OVERRIDES) {
  bach_id %in% names(channel_keep)
}

lookup_keep_override <- function(bach_id, channel_keep = PIPELINE_CHANNEL_KEEP_OVERRIDES) {
  if (!has_keep_override(bach_id, channel_keep)) {
    return(NULL)
  }

  channel_keep[[bach_id]]
}

is_excluded_edf <- function(
  edf_path,
  channel_keep = PIPELINE_CHANNEL_KEEP_OVERRIDES
) {
  bach_id <- infer_bach_id(edf_path)
  matched <- lookup_keep_override(bach_id, channel_keep)
  has_keep_override(bach_id, channel_keep) && is_excluded_keep_override(matched)
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

  if (!has_keep_override(bach_id, channel_keep)) {
    return(unique(c(default_keep_channels, mandatory_keep_channels)))
  }

  matched <- lookup_keep_override(bach_id, channel_keep)

  if (is_excluded_keep_override(matched)) {
    return(character())
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

get_filtered_annots_path <- function(filtered_edf_paths) {
  annot_paths <- filtered_edf_paths[grepl("\\.annots$", filtered_edf_paths)]
  if (!length(annot_paths)) {
    stop("Expected filtered .annots path alongside filtered EDF output.")
  }

  annot_paths[[1]]
}

load_edf <- function(edf_path, annot_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (missing(annot_path) || is.null(annot_path) || !nzchar(annot_path)) {
    stop("load_edf() requires an explicit annotation path (.XML or .annots).")
  }

  ledf(edf_path, base_name, annots = annot_path)
}
