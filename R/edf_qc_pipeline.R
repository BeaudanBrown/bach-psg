library(luna)
library(data.table)

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
