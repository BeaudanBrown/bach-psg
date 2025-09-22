library(targets)
library(data.table)
library(tarchetypes)
library(crew)

dotenv::load_dot_env()
edf_dir <- Sys.getenv("EDF_DIR")
data_dir <- Sys.getenv("DATA_DIR")
ncpus <- 3
# ncpus <- future::availableCores() - 1

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
      edfs <- list.files(edf_dir, pattern = "\\.edf$", full.names = TRUE)
      edfs[file.exists(paste0(edfs, ".XML"))]
    }
  ),
  tar_target(
    redcap_data,
    fread(file.path(data_dir, "data_cleaner.v2.csv"))
  ),
  ##########################
  # Threshold calculation
  ##########################
  tar_target(
    edf_results,
    process_edf(file.path(edf_dir, "filtered"), edf_files),
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
  # Branching constants
  ##########################
  tar_target(
    freqs,
    c(11, 15)
  ),
  tar_target(
    channels,
    c("C", "F")
  ),
  tar_target(
    outcomes,
    c("visualrepro2_total", "logicalmem_delay_total")
  ),
  tar_target(
    predictors,
    c("overlap", "angle", "mag")
  ),
  ##########################
  # Analysis pipeline
  ##########################
  tar_map(
    # Sleep stage masks
    values = data.table(
      mask = c("N2,N3", "N2", "N3"),
      name = c("N23", "N2", "N3")
    ),
    names = "name",
    # Run Luna to get spindle/SO data for given mask
    tar_target(
      stage_threshold_results,
      get_stage_spindles_with_threshold(
        file.path(edf_dir, "filtered"),
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
      channel_avg_dataset,
      average_channel_data(dataset),
      pattern = map(dataset)
    ),
    # Merge spindle/so data with redcap data
    tar_target(
      merged,
      merge(channel_avg_dataset, redcap_data, by = "ID", all.x = TRUE),
      pattern = map(channel_avg_dataset)
    ),
    # Fit model and extract parameter estimate/p val for all outcomes/predictors
    tar_target(
      model_summary,
      get_model_estimate(merged, outcomes, predictors),
      pattern = cross(cross(outcomes, predictors), merged)
    )
  )
)
