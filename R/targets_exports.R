build_export_targets <- function() {
  list(
    tar_target(
      psd_csv,
      {
        path <- file.path(
          data_dir,
          sprintf("psd_dt_%s.csv", filter_profile_names)
        )
        fwrite(psd_dt, path)
        path
      },
      pattern = map(psd_dt, filter_profile_names),
      format = "file"
    ),
    tar_target(
      qc_csv,
      {
        path <- file.path(data_dir, "qc_dt.csv")
        qc_dt <- build_qc_csv_rows(qc_results)
        fwrite(qc_dt, path)
        path
      },
      format = "file"
    )
  )
}
