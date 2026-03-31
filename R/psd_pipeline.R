get_qc <- function(
  filtered_edf_paths
) {
  # filtered_edf_paths may contain both .edf and .annots paths
  # Extract just the .edf file
  filtered_edf_path <- filtered_edf_paths[grepl("\\.edf$", filtered_edf_paths)]
  base_name <- infer_bach_id(filtered_edf_path)
  filter_profile <- infer_filter_profile(filtered_edf_path)
  result <- data.table(
    bach_id = base_name,
    filter_profile = filter_profile
  )
  load_edf(filtered_edf_path)

  qc <- leval("QC eeg=C3_M2,C4_M1 epoch")
  result$qc <- list(extract_luna_table(qc, "CH_EEG"))
  lrefresh()
  result
}

get_psd_results <- function(
  filtered_edf_paths,
  sleep_stage
) {
  # filtered_edf_paths may contain both .edf and .annots paths
  # Extract just the .edf file
  filtered_edf_path <- filtered_edf_paths[grepl("\\.edf$", filtered_edf_paths)]
  base_name <- infer_bach_id(filtered_edf_path)
  filter_profile <- infer_filter_profile(filtered_edf_path)
  result <- data.table(
    bach_id = base_name,
    filter_profile = filter_profile
  )
  load_edf(filtered_edf_path)

  leval(paste0("MASK ifnot=", sleep_stage, " & RE"))
  psd <- leval("PSD spectrum epoch")
  result$psd_b_ch <- list(extract_luna_table(psd, "B_CH"))
  lrefresh()
  result
}
