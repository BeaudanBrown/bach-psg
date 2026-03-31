build_qc_targets <- function() {
  list(
    tar_target(
      line_noise_review,
      get_line_noise_review(edf_files),
      pattern = map(edf_files)
    ),
    tar_target(
      line_noise_summary_branch,
      line_noise_review$summary,
      pattern = map(line_noise_review)
    ),
    tar_target(
      line_noise_spectra_branch,
      line_noise_review$spectra,
      pattern = map(line_noise_review)
    ),
    tar_target(
      line_noise_summary,
      collect_data_tables(line_noise_summary_branch)
    ),
    tar_target(
      line_noise_spectra,
      collect_data_tables(line_noise_spectra_branch)
    ),
    tar_target(
      raw_qc_results,
      get_raw_qc(edf_files, xml_files),
      pattern = map(edf_files, xml_files)
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
