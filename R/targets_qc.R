build_qc_targets <- function() {
  list(
    tar_target(
      qc_results,
      get_qc(filtered_edf_files),
      pattern = map(filtered_edf_files)
    ),
    tar_target(
      qc_all_dt,
      build_qc_csv_rows(qc_results)
    ),
    tar_target(
      qc_dt,
      {
        if (!nrow(qc_all_dt) || !"filter_profile" %in% names(qc_all_dt)) {
          data.table()
        } else {
          qc_all_dt[filter_profile == filter_profile_names]
        }
      },
      pattern = map(filter_profile_names)
    ),
    tar_target(
      qc_plot_specs,
      build_qc_plot_specs()
    )
  )
}
