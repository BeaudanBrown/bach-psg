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
    "data.table",
    "ggplot2",
    "cowplot",
    "grid"
  ),
  controller = crew_controller_local(
    workers = ncpus
  ),
  format = "qs"
)

tar_source()

list(
  build_input_targets(),
  build_cleaning_targets(),
  build_qc_targets(),
  build_branching_targets(),
  build_psd_targets(),
  build_spindle_targets(),
  build_analysis_targets(),
  build_export_targets()
) |> unlist(recursive = FALSE)
