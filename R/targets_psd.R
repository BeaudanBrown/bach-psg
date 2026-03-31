build_psd_targets <- function() {
  list(
    tar_target(
      psd_results,
      {
        result <- get_psd_results(
          filtered_edf_files,
          sleep_stage = sleep_stage
        )
        result$sleep_stage <- sleep_stage
        result
      },
      pattern = cross(filtered_edf_files, sleep_stage)
    ),
    tar_target(
      psd_dt,
      {
        collect_psd_b_ch(psd_results)
      }
    )
  )
}
