build_qc_targets <- function() {
  list(
    tar_target(
      raw_qc,
      get_raw_qc_data(edf_files, xml_files),
      pattern = map(edf_files, xml_files)
    ),
    tar_target(
      raw_qc_epoch,
      raw_qc$epoch_qc,
      pattern = map(raw_qc)
    ),
    tar_target(
      raw_qc_channel,
      raw_qc$channel_qc,
      pattern = map(raw_qc)
    ),
    tar_target(
      qc_epoch_dt,
      collect_data_tables(raw_qc_epoch)
    ),
    tar_target(
      qc_channel_dt,
      collect_data_tables(raw_qc_channel)
    ),
    tar_target(
      qc_summary,
      summarize_epoch_qc(qc_epoch_dt)
    ),
    tar_target(
      edge_epoch_review,
      get_edge_epoch_review(qc_epoch_dt)
    ),
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
      qc_results,
      get_qc(filtered_edf_files),
      pattern = map(filtered_edf_files)
    )
  )
}
