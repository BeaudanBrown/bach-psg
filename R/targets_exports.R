build_export_targets <- function() {
  list(
    tar_target(
      raw_qc_csv,
      {
        output_dir <- build_output_dir(data_dir, "raw")
        path <- file.path(output_dir, "qc_dt.csv")
        fwrite(raw_qc_dt, path)
        path
      },
      format = "file"
    ),
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
    ),
    tar_target(
      qc_density_plot_files,
      plot_qc_density(
        qc_all_dt = qc_all_dt,
        metric = qc_plot_specs$metric,
        channel = qc_plot_specs$channel,
        data_dir = data_dir
      ),
      pattern = map(qc_plot_specs),
      format = "file"
    )
  )
}
