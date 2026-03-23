library(luna)
library(data.table)

artifact_variant_dir <- function(base_dir, include_artifact_re) {
  file.path(base_dir, if (include_artifact_re) "with_re" else "current")
}

validate_edf_pair <- function(edf_path) {
  xml_path <- paste0(edf_path, ".XML")

  if (!file.exists(edf_path)) {
    stop(sprintf("EDF not found: %s", edf_path), call. = FALSE)
  }
  if (!file.exists(xml_path)) {
    stop(sprintf("Matching XML not found: %s", xml_path), call. = FALSE)
  }

  dataset_id <- tools::file_path_sans_ext(basename(edf_path))
  tryCatch(
    {
      ledf(edf_path, dataset_id, xml_path)
      lrefresh()
      invisible(xml_path)
    },
    error = function(err) {
      stop(
        sprintf(
          "Unable to load EDF/XML pair with Luna: %s\nUse a full, structurally valid EDF recording for artifact verification.",
          conditionMessage(err)
        ),
        call. = FALSE
      )
    }
  )
}

write_filtered_edf_variant <- function(
  edf_path,
  base_dir = file.path(tempdir(), "luna-artifact-verification"),
  include_artifact_re = FALSE,
  force = FALSE
) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")
  variant_dir <- artifact_variant_dir(base_dir, include_artifact_re)
  filtered_path <- file.path(variant_dir, paste0(base_name, ".edf"))
  annot_path <- file.path(variant_dir, paste0(base_name, ".annots"))

  dir.create(variant_dir, recursive = TRUE, showWarnings = FALSE)

  if (force) {
    unlink(c(filtered_path, annot_path))
  }

  if (!file.exists(filtered_path)) {
    ledf(edf_path, base_name, xml_path)
    leval(build_filtered_edf_command(
      filtered_dir = variant_dir,
      base_name = base_name,
      include_artifact_re = include_artifact_re
    ))
    lrefresh()
  }

  filtered_path
}

inspect_epoch_state <- function(dataset_path, xml_path, stage_mask = NULL) {
  dataset_id <- tools::file_path_sans_ext(basename(dataset_path))
  ledf(dataset_path, dataset_id, xml_path)

  if (!is.null(stage_mask)) {
    leval(sprintf("MASK ifnot=%s & RE", stage_mask))
  } else {
    leval("EPOCH")
  }

  epoch_map <- extract_luna_table(
    leval("EPOCH verbose"),
    "E",
    cols = c("E", "E1", "START", "STOP", "MID")
  )
  epoch_mask <- extract_luna_table(
    leval("DUMP-MASK"),
    "E",
    cols = c("E", "EMASK")
  )
  lrefresh()

  if (nrow(epoch_map) && nrow(epoch_mask)) {
    epoch_map <- merge(epoch_map, epoch_mask, by = "E", all.x = TRUE)
  }

  if (nrow(epoch_map)) {
    if (!"E1" %in% names(epoch_map)) {
      epoch_map[, E1 := E]
    }
    epoch_map[is.na(E1), E1 := E]
  }

  ensure_dt_cols(epoch_map, c("E", "E1", "START", "STOP", "MID", "EMASK"))
}

summarize_artifact_variant <- function(epoch_map, raw_masked_epochs) {
  dt <- ensure_dt_cols(copy(as.data.table(epoch_map)), c("E", "E1", "EMASK"))
  dt[, raw_epoch := fifelse(is.na(E1), E, E1)]

  data.table(
    n_epochs = uniqueN(dt$E),
    n_raw_epochs_retained = uniqueN(dt$raw_epoch),
    n_raw_masked_epochs_retained = uniqueN(dt$raw_epoch[dt$raw_epoch %in% raw_masked_epochs]),
    n_dataset_masked_epochs = uniqueN(dt$E[coalesce_zero(dt$EMASK) == 1])
  )
}

compare_artifact_variants <- function(
  edf_path,
  sleep_stage = c("N2", "N3"),
  base_dir = file.path(tempdir(), "luna-artifact-verification"),
  force = FALSE
) {
  sleep_stage <- match.arg(sleep_stage)
  xml_path <- validate_edf_pair(edf_path)
  qc <- get_raw_qc_data(edf_path)
  raw_epoch_qc <- ensure_dt_cols(
    qc$epoch_qc,
    c("E", "EMASK", "MASK", "CHEP", "BETA_MASK", "DELTA_MASK")
  )
  raw_masked_epochs <- sort(unique(raw_epoch_qc$E[
    coalesce_zero(raw_epoch_qc$EMASK) == 1 |
      coalesce_zero(raw_epoch_qc$MASK) == 1 |
      coalesce_zero(raw_epoch_qc$CHEP) == 1 |
      coalesce_zero(raw_epoch_qc$BETA_MASK) == 1 |
      coalesce_zero(raw_epoch_qc$DELTA_MASK) == 1
  ]))

  current_path <- write_filtered_edf_variant(
    edf_path = edf_path,
    base_dir = base_dir,
    include_artifact_re = FALSE,
    force = force
  )
  re_path <- write_filtered_edf_variant(
    edf_path = edf_path,
    base_dir = base_dir,
    include_artifact_re = TRUE,
    force = force
  )

  current_all_epochs <- inspect_epoch_state(current_path, xml_path)
  re_all_epochs <- inspect_epoch_state(re_path, xml_path)
  current_stage_epochs <- inspect_epoch_state(current_path, xml_path, stage_mask = sleep_stage)
  re_stage_epochs <- inspect_epoch_state(re_path, xml_path, stage_mask = sleep_stage)

  summary <- rbindlist(list(
    cbind(
      variant = "raw_qc",
      stage = "all",
      data.table(
        n_epochs = uniqueN(raw_epoch_qc$E),
        n_raw_epochs_retained = uniqueN(raw_epoch_qc$E),
        n_raw_masked_epochs_retained = uniqueN(raw_masked_epochs),
        n_dataset_masked_epochs = uniqueN(raw_epoch_qc$E[coalesce_zero(raw_epoch_qc$EMASK) == 1])
      )
    ),
    cbind(
      variant = "current",
      stage = "all",
      summarize_artifact_variant(current_all_epochs, raw_masked_epochs)
    ),
    cbind(
      variant = "current",
      stage = sleep_stage,
      summarize_artifact_variant(current_stage_epochs, raw_masked_epochs)
    ),
    cbind(
      variant = "with_re",
      stage = "all",
      summarize_artifact_variant(re_all_epochs, raw_masked_epochs)
    ),
    cbind(
      variant = "with_re",
      stage = sleep_stage,
      summarize_artifact_variant(re_stage_epochs, raw_masked_epochs)
    )
  ), fill = TRUE)

  summary[, `:=`(
    edf = basename(edf_path),
    raw_masked_epoch_total = uniqueN(raw_masked_epochs),
    sleep_stage = sleep_stage
  )]
  setcolorder(summary, c(
    "edf",
    "variant",
    "stage",
    "sleep_stage",
    "n_epochs",
    "n_raw_epochs_retained",
    "raw_masked_epoch_total",
    "n_raw_masked_epochs_retained",
    "n_dataset_masked_epochs"
  ))

  list(
    summary = summary[],
    raw_qc = raw_epoch_qc,
    current_all_epochs = current_all_epochs,
    current_stage_epochs = current_stage_epochs,
    with_re_all_epochs = re_all_epochs,
    with_re_stage_epochs = re_stage_epochs,
    paths = list(
      current = current_path,
      with_re = re_path
    )
  )
}
