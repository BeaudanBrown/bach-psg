library(targets)
library(data.table)
library(tarchetypes)
library(crew)

dotenv::load_dot_env()
#data_dir <- Sys.getenv("DATA_DIR")
edf_dir <- Sys.getenv("EDF_DIR")
ncpus <- 3

# # Ensure single threaded within targets
# Sys.setenv(R_DATATABLE_NUM_THREADS = 1)
# Sys.setenv(OMP_NUM_THREADS = 1)
# Sys.setenv(MKL_NUM_THREADS = 1)
# Sys.setenv(OPENBLAS_NUM_THREADS = 1)

# Set target options:
tar_option_set(
  packages = c(),
  controller = crew_controller_local(
  workers = ncpus
),
  format = "qs"
)

# Run the R scripts in the R/ folder
tar_source()

## pipeline
list(
  tar_target(
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
     EMPTH_MEDIANS,
     get_empirical_threshold(edf_results),
     pattern = map(edf_results)
   ),
  tar_target(
    threshold,
    median(EMPTH_MEDIANS)
  ),
  tar_target(
    threshold_results,
    get_spindles_with_threshold(file.path(edf_dir, "filtered"), edf_files, threshold),
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
  )
)

