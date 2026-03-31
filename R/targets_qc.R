build_qc_targets <- function() {
  list(
    tar_target(
      raw_qc_results,
      get_raw_qc(raw_edf_inputs$edf_path, raw_edf_inputs$xml_path),
      pattern = map(raw_edf_inputs)
    ),
    tar_target(
      raw_qc_dt,
      build_qc_csv_rows(raw_qc_results)
    ),
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
    )
  )
}
