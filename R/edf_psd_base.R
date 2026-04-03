process_edf <- function(filtered_edf_paths) {
  # filtered_edf_paths may contain both .edf and .annots paths
  # Extract just the .edf file
  filtered_edf_path <- filtered_edf_paths[grepl("\\.edf$", filtered_edf_paths)]
  filtered_annot_path <- get_filtered_annots_path(filtered_edf_paths)
  base_name <- infer_bach_id(filtered_edf_path)
  filter_profile <- infer_filter_profile(filtered_edf_path)
  load_edf(filtered_edf_path, filtered_annot_path)

  result <- data.table(
    bach_id = base_name,
    filter_profile = filter_profile
  )

  leval(sprintf(
    "MASK ifnot=%s & RE",
    paste(PIPELINE_SLEEP_STAGES, collapse = ",")
  ))
  result$psd <- leval("PSD spectrum epoch")
  result$spindles <- leval(
    "SPINDLES fc=11,15 empirical set-empirical median"
  )
  lrefresh()
  result
}
