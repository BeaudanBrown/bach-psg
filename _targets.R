library(targets)
library(data.table)
library(tarchetypes)
library(crew)

dotenv::load_dot_env()
edf_dir <- Sys.getenv("EDF_DIR")
data_dir <- Sys.getenv("DATA_DIR")
# ncpus <- 3
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
  tar_target(
    threshold_results,
    get_spindles_with_threshold(
      file.path(edf_dir, "filtered"),
      edf_files,
      threshold
    ),
    pattern = map(edf_files)
  ),
  tar_target(
    stages,
    c("N2,N3", "N2", "N3")
  ),
  tar_target(
    outcomes,
    c("visualrepro2_total", "logicalmem_delay_total")
  ),
  tar_target(
    predictors,
    c("overlap", "angle", "mag")
  ),
  tar_map(
    values = data.table(
      stage = c("N2,N3", "N2", "N3"),
      name = c("N23", "N2", "N3")
    ),
    names = "name",
    tar_target(
      stage_threshold_results,
      get_stage_spindles_with_threshold(
        file.path(edf_dir, "filtered"),
        edf_files,
        threshold,
        sleep_stage = stage
      ),
      pattern = map(edf_files)
    ),
    tar_target(
      filtered_results,
      filter_spindles_so(stage_threshold_results),
      pattern = map(stage_threshold_results)
    ),
    tar_target(
      cleaned_results,
      clean_angle(filtered_results),
      pattern = map(filtered_results)
    ),
    tar_target(
      dataset,
      cleaned_results[F == freqs & grepl(channels, CH), ],
      pattern = cross(freqs, channels)
    ),
    tar_target(
      channel_avg_dataset,
      average_channel_data(dataset),
      pattern = map(dataset)
    ),
    tar_target(
      md,
      {
        setnames(channel_avg_dataset, "bach_id", "ID")
        merge(channel_avg_dataset, redcap_data, by = "ID", all.x = TRUE)
      },
      pattern = map(channel_avg_dataset)
    ),
    tar_target(
      model_summary,
      {
        formula_str <- paste0(
          outcomes,
          " ~ ",
          predictors,
          " + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso"
        )
        model <- lm(as.formula(formula_str), data = md)
        summary <- summary(model)
        result <- as.data.table(summary$coefficients, keep.rownames = TRUE)[
          rn == predictors,
        ]
        setnames(result, "rn", "predictor")
        md[, list(ID, freq, channel)]
        result[,
          c("outcome", "freq", "channel", "n") := list(
            outcomes,
            unique(md$freq),
            unique(md$channel),
            sum(!is.na(md[[predictors]]))
          )
        ]
      },
      pattern = cross(cross(outcomes, predictors), md)
    )
  )
)
