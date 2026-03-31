QC_PLOT_METRICS <- c(
  "PROP_FLAG",
  "P_AMP",
  "P_CLIP",
  "P_FLAT",
  "P_HF",
  "P_HJORTH",
  "P_LN",
  "P_PEAK"
)

QC_PLOT_CHANNELS <- c("C3_M2", "C4_M1", "average_channels")

build_qc_plot_specs <- function() {
  data.table::CJ(
    metric = QC_PLOT_METRICS,
    channel = QC_PLOT_CHANNELS,
    sorted = FALSE
  )
}

get_qc_plot_data <- function(qc_all_dt, metric, channel) {
  if (!nrow(qc_all_dt)) {
    return(data.table())
  }

  if (identical(channel, "average_channels")) {
    dt <- qc_all_dt[
      ,
      .(metric_value = mean(get(metric), na.rm = TRUE)),
      by = .(bach_id, filter_profile)
    ]
    dt[, channel_label := "Average of C3/C4"]
    return(dt[is.finite(metric_value)])
  }

  dt <- copy(qc_all_dt[CH == channel])
  if (!nrow(dt)) {
    return(data.table())
  }
  dt[, metric_value := get(metric)]
  dt[, channel_label := channel]
  dt[is.finite(metric_value), .(bach_id, filter_profile, metric_value, channel_label)]
}

build_qc_plot_path <- function(data_dir, metric, channel) {
  plot_dir <- file.path(data_dir, "qc_plots", metric)
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir, recursive = TRUE)
  }
  file.path(plot_dir, paste0(channel, ".png"))
}

plot_qc_density <- function(qc_all_dt, metric, channel, data_dir) {
  plot_dt <- get_qc_plot_data(qc_all_dt, metric, channel)
  path <- build_qc_plot_path(data_dir, metric, channel)

  if (!nrow(plot_dt)) {
    grDevices::png(path, width = 1800, height = 1200, res = 200)
    grid::grid.newpage()
    grid::grid.text(sprintf("No data available for %s (%s)", metric, channel))
    grDevices::dev.off()
    return(path)
  }

  profile_levels <- names(PIPELINE_FILTER_PROFILES)
  legend_title <- "Filter"
  if (metric == "P_LN") {
    zero_summary <- plot_dt[
      ,
      .(
        zero_prop = mean(metric_value == 0, na.rm = TRUE)
      ),
      by = filter_profile
    ]
    legend_labels <- setNames(
      sprintf(
        "%s (%s zero)",
        zero_summary$filter_profile,
        scales::percent(zero_summary$zero_prop, accuracy = 0.1)
      ),
      zero_summary$filter_profile
    )
    plot_dt <- plot_dt[metric_value > 0]
    legend_title <- "Filter (% zero)"
  } else {
    legend_labels <- setNames(profile_levels, profile_levels)
  }

  if (!nrow(plot_dt)) {
    grDevices::png(path, width = 1800, height = 1200, res = 200)
    grid::grid.newpage()
    grid::grid.text(sprintf("No non-zero data available for %s (%s)", metric, channel))
    grDevices::dev.off()
    return(path)
  }

  plot_dt[, filter_profile := factor(filter_profile, levels = profile_levels)]

  profile_palette <- c(
    base = "#1B4965",
    bandpass_0_3_35 = "#CA6702",
    notch_50 = "#2A9D8F"
  )

  x_label <- if (metric == "PROP_FLAG") {
    "Flagged epoch proportion"
  } else {
    gsub("_", " ", metric)
  }

  p <- ggplot2::ggplot(
    plot_dt,
    ggplot2::aes(x = metric_value, colour = filter_profile, fill = filter_profile)
  ) +
    ggplot2::geom_density(alpha = 0.18, linewidth = 1) +
    ggplot2::scale_colour_manual(values = profile_palette, drop = FALSE, labels = legend_labels) +
    ggplot2::scale_fill_manual(values = profile_palette, drop = FALSE, labels = legend_labels) +
    ggplot2::labs(
      title = sprintf("%s density", x_label),
      subtitle = plot_dt$channel_label[[1]],
      x = x_label,
      y = "Density",
      colour = legend_title,
      fill = legend_title
    ) +
    cowplot::theme_cowplot(font_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(path, plot = p, width = 9, height = 6, dpi = 200)
  path
}
