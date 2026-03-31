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

build_filtered_edf_command <- function(
  filtered_dir,
  base_name,
  filter_profile = NULL
) {
  profile <- list(
    filter_commands = character(),
    qc_commands = PIPELINE_DEFAULT_QC_COMMANDS
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

  filter_commands <- vapply(
    profile$filter_commands,
    as.character,
    character(1),
    USE.NAMES = FALSE
  )
  qc_commands <- vapply(
    profile$qc_commands,
    as.character,
    character(1),
    USE.NAMES = FALSE
  )

  filter_chain <- paste(filter_commands, collapse = " & ")
  qc_chain <- paste(qc_commands, collapse = " & ")
  pre_artifact_filter_step <- if (length(filter_commands)) {
    paste0(filter_chain, " &")
  } else {
    ""
  }

  sprintf(
    "EPOCH &
    SUPPRESS-ECG ecg=ECG &
    SIGNALS keep=${eeg} &
    MASK clear &
    EDGER sig=${eeg} epoch mask &
    %s
    ARTIFACTS &
    SIGSTATS &
    %s &
    CHEP epoch &
    DUMP-MASK annot=artifacts &
    QC eeg=C3_M2,C4_M1 &
    WRITE-ANNOTS file=%s/%s.annots annot=artifacts &
    WRITE edf-dir=%s edf=%s",
    pre_artifact_filter_step,
    qc_chain,
    filtered_dir,
    base_name,
    filtered_dir,
    base_name
  )
}

create_filtered_edf <- function(edf_path, xml_path = NULL, filter_profile_name = "base", filter_profile = NULL) {
  filtered_dir <- file.path(dirname(edf_path), "filtered", filter_profile_name)
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

load_edf <- function(edf_path, xml_path = NULL) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    # Assume XML is named like the original EDF (not the filtered one)
    # For filtered EDFs, the XML is still with the original
    raw_edf_dir <- if (grepl("/filtered", dirname(edf_path), perl = TRUE)) {
      sub("/filtered(?:/[^/]+)?$", "", dirname(edf_path), perl = TRUE)
    } else {
      dirname(edf_path)
    }
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
