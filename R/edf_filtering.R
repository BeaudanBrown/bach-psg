library(luna)

build_filter_profile_specs <- function(filter_profiles) {
  if (!is.list(filter_profiles) || is.null(names(filter_profiles))) {
    stop("filter_profiles must be a named list")
  }

  specs <- lapply(names(filter_profiles), function(filter_profile_name) {
    list(
      filter_profile_name = filter_profile_name,
      filter_profile = filter_profiles[[filter_profile_name]]
    )
  })
  setNames(specs, names(filter_profiles))
}

build_filtered_edf_command <- function(
  filtered_dir,
  base_name,
  filter_profile = NULL
) {
  profile <- list(
    filter_commands = PIPELINE_DEFAULT_FILTER_COMMANDS
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
    filter_commands <- PIPELINE_DEFAULT_FILTER_COMMANDS
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

create_filtered_edf_from_spec <- function(edf_path, filter_profile_spec) {
  if (!is.list(filter_profile_spec)) {
    stop("filter_profile_spec must be a list")
  }

  if (is.null(filter_profile_spec$filter_profile_name)) {
    stop("filter_profile_spec$filter_profile_name is required")
  }
  if (is.null(filter_profile_spec$filter_profile)) {
    stop("filter_profile_spec$filter_profile is required")
  }

  create_filtered_edf(
    edf_path = edf_path,
    filter_profile_name = filter_profile_spec$filter_profile_name,
    filter_profile = filter_profile_spec$filter_profile
  )
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
