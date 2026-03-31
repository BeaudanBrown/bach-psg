build_export_targets <- function() {
  list(
    tar_target(
      psd_csv,
      {
        path <- file.path(data_dir, paste0("psd_dt_", filter_profile_names, ".csv"))
        fwrite(psd_dt, path)
        path
      },
      pattern = map(psd_dt, filter_profile_names),
      format = "file"
    ),
    tar_target(
      qc_csv,
      {
        path <- file.path(data_dir, paste0("qc_dt_", filter_profile_names, ".csv"))
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
