build_export_targets <- function() {
  list(
    tar_target(
      psd_csv,
      {
        output_dir <- build_output_dir(data_dir, filter_profile_names)
        path <- file.path(output_dir, "psd_dt.csv")
        fwrite(psd_dt, path)
        path
      },
      pattern = map(psd_dt, filter_profile_names),
      format = "file"
    ),
    tar_target(
      qc_csv,
      {
        output_dir <- build_output_dir(data_dir, filter_profile_names)
        path <- file.path(output_dir, "qc_dt.csv")
        fwrite(qc_dt, path)
        path
      },
      pattern = map(qc_dt, filter_profile_names),
      format = "file"
    )
  )
}
