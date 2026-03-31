build_cleaning_targets <- function() {
  list(
    tar_target(
      filter_profiles,
      PIPELINE_FILTER_PROFILES
    ),
    tar_target(
      filter_profile_names,
      names(filter_profiles)
    ),
    tar_target(
      filter_profile_index,
      seq_along(filter_profile_names)
    ),
    tar_target(
      channel_dropped_edf_files,
      create_channel_dropped_edf(
        edf_path = raw_edf_inputs$edf_path,
        xml_path = raw_edf_inputs$xml_path,
        drop_channels = raw_edf_inputs$drop_channels
      ),
      pattern = map(raw_edf_inputs),
      format = "file"
    ),
    tar_target(
      filtered_edf_files,
      create_filtered_edf(
        edf_path = channel_dropped_edf_files,
        xml_path = raw_edf_inputs$xml_path,
        filter_profile_name = filter_profile_names[filter_profile_index],
        filter_profile = filter_profiles[[filter_profile_index]]
      ),
      pattern = cross(map(channel_dropped_edf_files, raw_edf_inputs), filter_profile_index),
      format = "file"
    ),
    tar_target(
      edf_results,
      process_edf(filtered_edf_files),
      pattern = map(filtered_edf_files)
    ),
    tar_target(
      per_ppt_thresholds,
      get_empirical_threshold(edf_results),
      pattern = map(edf_results)
    ),
    tar_target(
      per_profile_thresholds,
      get_empirical_threshold_by_profile(edf_results)
    ),
    tar_target(
      threshold,
      median(per_ppt_thresholds, na.rm = TRUE)
    )
  )
}
