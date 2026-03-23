# Development

## Local Setup

1. Enter the dev shell:

```bash
nix develop
```

2. Create a local `.env` from [`.env.example`](../.env.example):

```bash
cp .env.example .env
```

3. Set:

- `EDF_DIR` to a directory containing BACH EDF files and paired `.XML`
  annotation files.
- `DATA_DIR` to a directory containing tabular study inputs and receiving
  generated CSV outputs.

## Common Commands

```bash
just manifest
just run
just psd-csv
just artifact-verify /absolute/path/to/BACH0001.edf N2
```

## Structure

- `_targets.R` defines the graph.
- `R/utils.R` contains table and summary helpers.
- `R/edf_processing.R` contains Luna-loading and EDF/QC extraction helpers.
- `R/qc.R` contains QC summaries and review tables.
- `R/analysis.R` contains spindle/SO transforms and model helpers.
- `R/artifact_verification.R` contains non-destructive checks for whether the
  current preprocessing path actually excludes artifact-masked epochs.

## Boundary

Only files required for the maintained `{targets}` pipeline should remain
tracked here. Ad hoc regression scripts, old serialized snapshots, and other
non-pipeline assets should stay out of the repo unless they become part of the
supported workflow.
