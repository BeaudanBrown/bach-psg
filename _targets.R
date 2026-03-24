library(targets)
library(data.table)
library(tarchetypes)
library(crew)

dotenv::load_dot_env()
edf_dir <- Sys.getenv("EDF_DIR")
data_dir <- Sys.getenv("DATA_DIR")
ncpus <- future::availableCores() - 1

# Ensure single threaded within targets
Sys.setenv(R_DATATABLE_NUM_THREADS = 1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(MKL_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)

# Set target options:
tar_option_set(
  packages = c(
    "luna",
    "data.table"
  ),
  controller = crew_controller_local(
    workers = ncpus
  ),
  format = "qs"
)

tar_source()

list(
  ##########################
  # Raw data
  ##########################
  tar_files(
    edf_files,
    {
      edfs <- list.files(edf_dir, pattern = "BACH\\d{4}\\.edf$", full.names = TRUE)
      edfs[file.exists(paste0(edfs, ".XML"))]
    }
  ),
  tar_target(
    redcap_data,
    fread(file.path(data_dir, "data_cleaner.v2.csv"))
  ),
  ##########################
  # Filtered EDFs (artifact-cleaned)
  ##########################
  tar_target(
    filtered_edf_files,
    create_filtered_edf(edf_files),
    pattern = map(edf_files),
    format = "file"
  ),
  ##########################
  # Threshold calculation
  ##########################
  tar_target(
    edf_results,
    process_edf(edf_files),
    pattern = map(edf_files)
  ),
  tar_target(
    per_ppt_thresholds,
    get_empirical_threshold(edf_results),
    pattern = map(edf_results)
  ),
  tar_target(
    threshold,
    median(per_ppt_thresholds, na.rm = TRUE)
  ),
  ##########################
  # QC pipeline on raw EDFs
  ##########################
  tar_target(
    raw_qc,
    get_raw_qc_data(edf_files),
    pattern = map(edf_files)
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
    rbindlist(
      collect_tables(raw_qc_epoch),
      fill = TRUE
    )
  ),
  tar_target(
    qc_channel_dt,
    rbindlist(
      collect_tables(raw_qc_channel),
      fill = TRUE
    )
  ),
  tar_target(
    qc_summary,
    summarize_epoch_qc(qc_epoch_dt)
  ),
  # Manual review target:
  # inspect leading/trailing epochs before adding explicit edge trimming.
  # If exclusions cluster at the start or end of the night, decide whether
  # you want a fixed edge-trim rule in preprocessing.
  tar_target(
    edge_epoch_review,
    get_edge_epoch_review(qc_epoch_dt)
  ),
  # Manual review target:
  # inspect spectra for sharp 50 Hz peaks or harmonics before enabling any
  # denoising step by default. This Luna build exposes PSD review cleanly,
  # but line-denoising itself should only be integrated after confirming
  # mains contamination is material in your recordings.
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
    rbindlist(
      collect_tables(line_noise_summary_branch),
      fill = TRUE
    )
  ),
  tar_target(
    line_noise_spectra,
    rbindlist(
      collect_tables(line_noise_spectra_branch),
      fill = TRUE
    )
  ),
  ##########################
  # Branching constants
  ##########################
  tar_target(
    freqs,
    c(11, 15)
  ),
  tar_target(
    channels,
    c("C3", "C4")
  ),
  tar_target(
    outcomes,
    c("visualrepro2_total", "logicalmem_delay_total")
  ),
  tar_target(
    predictors,
    c("overlap", "angle", "mag")
  ),
  tar_target(
    moderators,
    c("ab4240ratio_plasma", "ptau217_mean_conc_plasma", "gfap_mean_conc_plasma")
  ),
  ##########################
  # Analysis pipeline
  ##########################
  tar_map(
    # Sleep stage masks
    values = data.table(
      mask = c("N2", "N3"),
      name = c("N2", "N3")
    ),
    names = "name",
    # Run Luna to get spindle/SO data for given mask
    tar_target(
      stage_threshold_results,
      get_stage_spindles_with_threshold(
        edf_files,
        threshold,
        sleep_stage = mask
      ),
      pattern = map(edf_files)
    ),
    # QC/sensitivity branch:
    # run stage-restricted spindle detection on the raw EDF without
    # restructuring so noisy-versus-clean epochs can be compared.
    tar_target(
      raw_stage_threshold_results,
      get_raw_stage_spindles_with_threshold(
        edf_files,
        threshold,
        sleep_stage = mask
      ),
      pattern = map(edf_files)
    ),
    # Combine spindle and SO data into a data.table with relevant columns
    tar_target(
      filtered_results,
      filter_spindles_so(stage_threshold_results),
      pattern = map(stage_threshold_results)
    ),
    tar_target(
      cleaned_spindle_epochs,
      extract_spindle_epoch_counts(stage_threshold_results, sleep_stage = mask),
      pattern = map(stage_threshold_results)
    ),
    tar_target(
      raw_spindle_epochs,
      extract_spindle_epoch_counts(raw_stage_threshold_results, sleep_stage = mask),
      pattern = map(raw_stage_threshold_results)
    ),
    tar_target(
      raw_spindle_qc,
      compare_spindles_by_qc(raw_spindle_epochs, qc_epoch_dt, sleep_stage = mask),
      pattern = map(raw_spindle_epochs)
    ),
    # Filter coupl_angle based on coupl_mag_emp
    tar_target(
      cleaned_results,
      clean_angle(filtered_results),
      pattern = map(filtered_results)
    ),
    # Get dataset with each combination of frequency and channel
    tar_target(
      dataset,
      cleaned_results[F == freqs & grepl(channels, CH), ],
      pattern = cross(freqs, channels)
    ),
    # Unwrap angle data and average across the 2 electrodes/channel
    tar_target(
      wrapped_dataset,
      wrap_angle(dataset),
      pattern = map(dataset)
    ),
    # Merge spindle/so data with redcap data
    tar_target(
      merged,
      merge(wrapped_dataset, redcap_data, by = "ID", all.x = TRUE),
      pattern = map(wrapped_dataset)
    ),
    # make models
    tar_target(
      all_models,
      get_model(merged, outcomes, predictors, moderators),
      pattern = cross(cross(cross(outcomes, predictors), moderators), merged)
    ),
    # Fit model and extract parameter estimate/p val for all outcomes/predictors
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
    )
  ),
  tar_target(
    cleaned_spindle_epoch_dt,
    rbindlist(
      collect_tables(list(cleaned_spindle_epochs_N2, cleaned_spindle_epochs_N3)),
      fill = TRUE
    )
  ),
  tar_target(
    raw_spindle_epoch_dt,
    rbindlist(
      collect_tables(list(raw_spindle_epochs_N2, raw_spindle_epochs_N3)),
      fill = TRUE
    )
  ),
  tar_target(
    raw_spindle_qc_dt,
    rbindlist(
      collect_tables(list(raw_spindle_qc_N2, raw_spindle_qc_N3)),
      fill = TRUE
    )
  ),
  tar_target(
    raw_spindle_qc_summary,
    summarize_spindle_qc(raw_spindle_qc_dt)
  ),
  tar_target(
    qc_overview,
    build_qc_overview(
      qc_summary,
      raw_spindle_qc_summary,
      edge_epoch_review,
      line_noise_summary
    )
  ),
  tar_target(
    psd_dt,
    {
      stages <- list(
        N2 = stage_threshold_results_N2$psd,
        N3 = stage_threshold_results_N3$psd
      )
      rbindlist(
        lapply(names(stages), function(stage_name) {
          results <- lapply(stages[[stage_name]], function(ppt_data) {
            if (is.null(ppt_data) || is.null(ppt_data$B_CH)) {
              return(NULL)
            }
            ppt_data$B_CH$stage <- stage_name
            ppt_data$B_CH
          })
          rbindlist(Filter(Negate(is.null), results))
        })
      )

    }
  ),
  tar_target(
    psd_csv,
    {
      path <- file.path(data_dir, "psd_dt.csv")
      fwrite(psd_dt, path)
      path
    },
    format = "file"
  )
)
# combined sig model results into one data.table
#     tar_target(
#       model_dts,
#       c("N2_filtered", "N23_filtered")
#     ),
#     tar_target(
#       sig_model_results,
#       get_dt_sig_results(model_dts)
#     )
#   )
# )
