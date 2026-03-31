build_input_targets <- function() {
  list(
    tar_files(
      edf_files,
      {
        edfs <- list.files(edf_dir, pattern = "BACH\\d{4}\\.edf$", full.names = TRUE)
        edfs[file.exists(paste0(edfs, ".XML"))]
      }
    ),
    tar_target(
      xml_files,
      get_raw_xml_path(edf_files),
      pattern = map(edf_files),
      format = "file"
    ),
    tar_target(
      redcap_data,
      fread(file.path(data_dir, "data_cleaner.v2.csv"))
    ),
    tar_target(
      channel_exclusions,
      PIPELINE_CHANNEL_EXCLUSIONS
    ),
    tar_target(
      edf_channel_exclusions,
      lookup_channel_exclusions(edf_files, channel_exclusions),
      pattern = map(edf_files)
    ),
    tar_target(
      raw_input_summary_branch,
      get_raw_input_summary(edf_files, xml_files),
      pattern = map(edf_files, xml_files)
    ),
    tar_target(
      raw_input_summary,
      collect_data_tables(raw_input_summary_branch)
    )
  )
}
