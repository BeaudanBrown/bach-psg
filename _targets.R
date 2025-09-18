library(targets)
library(data.table)
library(tarchetypes)
library(crew)

dotenv::load_dot_env()
edf_dir <- Sys.getenv("EDF_DIR")
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

freqs <- data.table(
  freq = c(11, 15),
  name = c("slow", "fast")
)

channels <- data.table(
  prefix = c("C", "F"),
  name = c("central", "frontal")
)

predictors <- data.table(
  variable = c("COUPL_ANGLE", "COUPL_OVERLAP_Z", "COUPL_MAG_Z"),
  name = c("angle", "overlap", "mag")
)

list(
  tar_files(
    edf_files,
    {
      edfs <- list.files(edf_dir, pattern = "\\.edf$", full.names = TRUE)
      edfs[file.exists(paste0(edfs, ".XML"))]
    }
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
    filtered_results,
    filter_spindles_so(threshold_results),
    pattern = map(threshold_results)
  ),
  tar_target(
    cleaned_results,
    clean_angle(filtered_results),
    pattern = map(filtered_results)
  ),
  tar_map(
    values = freqs,
    names = name,
    tar_map(
      values = channels,
      names = name,

      tar_target(
        dataset,
        cleaned_results[F == freq & grepl(prefix, CH), ],
        pattern = map(cleaned_results)
      ),
      tar_target(
        final_dataset,
        {
          if (freq == 11) {
            dataset[,
              COUPL_ANGLE := ifelse(
                COUPL_ANGLE < 125,
                COUPL_ANGLE + 360,
                COUPL_ANGLE
              )
            ]
          }
          data.table(
            bach_id = unique(dataset$ID),
            channel = prefix,
            freq = freq,
            overlap = mean(dataset$COUPL_OVERLAP_Z, na.rm = TRUE),
            mag = mean(dataset$COUPL_MAG_Z, na.rm = TRUE),
            angle = mean(dataset$COUPL_ANGLE, na.rm = TRUE)
          )
        },
        pattern = map(dataset)
      )
    )
  )
)
