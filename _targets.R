library(targets)
library(data.table)
library(tarchetypes)

dotenv::load_dot_env()
data_dir <- Sys.getenv("DATA_DIR")
edf_dir <- Sys.getenv("EDF_DIR")
ncpus <- future::availableCores() - 1

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
    data_cleaner_file,
    file.path(data_dir, "data_cleaner_output.csv"),
    format = "file"
  ),
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
  )
)
