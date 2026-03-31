build_spindle_targets <- function() {
  list(
    tar_target(
      stage_threshold_results,
      {
        filter_profile <- infer_filter_profile(filtered_edf_files)
        threshold_value <- ifelse(
          is.null(per_profile_thresholds[[filter_profile]]) ||
            is.na(per_profile_thresholds[[filter_profile]]),
          median(per_ppt_thresholds, na.rm = TRUE),
          per_profile_thresholds[[filter_profile]]
        )
        result <- get_stage_spindles_with_threshold(
          filtered_edf_files,
          threshold_value,
          sleep_stage = sleep_stage
        )
        result$sleep_stage <- sleep_stage
        result$filter_profile <- filter_profile
        result
      },
      pattern = cross(filtered_edf_files, sleep_stage)
    ),
    tar_target(
      raw_stage_threshold_results,
      {
        result <- get_raw_stage_spindles_with_threshold(
          raw_edf_inputs$edf_path,
          raw_edf_inputs$xml_path,
          threshold,
          sleep_stage = sleep_stage
        )
        result$sleep_stage <- sleep_stage
        result
      },
      pattern = cross(map(raw_edf_inputs), sleep_stage)
    ),
    tar_target(
      filtered_results,
      filter_spindles_so(stage_threshold_results),
      pattern = map(stage_threshold_results)
    ),
    tar_target(
      cleaned_spindle_epochs,
      extract_spindle_epoch_counts(stage_threshold_results, sleep_stage = sleep_stage),
      pattern = map(stage_threshold_results)
    ),
    tar_target(
      raw_spindle_epochs,
      extract_spindle_epoch_counts(raw_stage_threshold_results, sleep_stage = sleep_stage),
      pattern = map(raw_stage_threshold_results)
    ),
    tar_target(
      cleaned_results,
      clean_angle(filtered_results),
      pattern = map(filtered_results)
    ),
    tar_target(
      cleaned_spindle_epoch_dt,
      collect_data_tables(cleaned_spindle_epochs)
    ),
    tar_target(
      raw_spindle_epoch_dt,
      collect_data_tables(raw_spindle_epochs)
    )
  )
}
