library(luna)
library(data.table)

get_raw_input_summary <- function(edf_path, xml_path = NULL) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    xml_path <- get_raw_xml_path(edf_path)
  }

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

get_raw_qc <- function(edf_path, xml_path = NULL) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  if (is.null(xml_path)) {
    xml_path <- get_raw_xml_path(edf_path)
  }

  result <- data.table(
    bach_id = base_name,
    filter_profile = "raw"
  )

  ledf(edf_path, base_name, annots = xml_path)
  result$qc <- leval("QC eeg=C3_M2,C4_M1 epoch")
  lrefresh()
  result
}
