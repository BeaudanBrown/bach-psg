default:
    @just --list

manifest:
    Rscript -e 'targets::tar_manifest()'

run:
    Rscript -e 'targets::tar_make()'

psd-csv:
    Rscript -e 'targets::tar_make(psd_csv)'

artifact-verify EDF STAGE="N2":
    Rscript -e 'source("R/utils.R"); source("R/edf_processing.R"); source("R/artifact_verification.R"); result <- compare_artifact_variants("{{EDF}}", sleep_stage = "{{STAGE}}", force = TRUE); print(result$summary)'
