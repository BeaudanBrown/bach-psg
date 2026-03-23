default:
    @just --list

manifest:
    Rscript -e 'targets::tar_manifest()'

run:
    Rscript -e 'targets::tar_make()'

psd-csv:
    Rscript -e 'targets::tar_make(psd_csv)'
