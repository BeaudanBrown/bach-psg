build_analysis_targets <- function() {
  list(
    tar_target(
      dataset,
      cleaned_results[F == freqs & grepl(channels, CH), ],
      pattern = cross(freqs, channels)
    ),
    tar_target(
      wrapped_dataset,
      wrap_angle(dataset),
      pattern = map(dataset)
    ),
    tar_target(
      merged,
      merge(wrapped_dataset, redcap_data, by = "ID", all.x = TRUE),
      pattern = map(wrapped_dataset)
    ),
    tar_target(
      all_models,
      get_model(merged, outcomes, predictors, moderators),
      pattern = cross(cross(cross(outcomes, predictors), moderators), merged)
    ),
    tar_target(
      model_summary,
      get_model_estimate(merged, outcomes, predictors),
      pattern = cross(cross(outcomes, predictors), merged)
    ),
    tar_target(
      moderation_summary,
      get_interaction_estimates(merged, outcomes, predictors, moderators),
      pattern = cross(cross(cross(outcomes, predictors), moderators), merged)
    ),
    tar_target(
      significant_results,
      moderation_summary[`Pr(>|t|)` < .05, ]
    ),
    tar_target(
      significant_model_results,
      model_summary[`Pr(>|t|)` < .05, ]
    ),
    tar_target(
      qc_overview,
      build_qc_overview(
        qc_summary,
        raw_spindle_qc_summary,
        edge_epoch_review,
        line_noise_summary
      )
    )
  )
}
