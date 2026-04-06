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
      channel_keep,
      PIPELINE_CHANNEL_KEEP_OVERRIDES
    ),
    tar_target(
      included_edf_files,
      edf_files[!vapply(edf_files, is_excluded_edf, logical(1), channel_keep = channel_keep)]
    ),
    tar_target(
      raw_edf_inputs,
      list(
        edf_path = included_edf_files,
        xml_path = get_raw_xml_path(included_edf_files),
        keep_channels = lookup_keep_channels(included_edf_files, channel_keep)
      ),
      pattern = map(included_edf_files)
    )
  )
}
