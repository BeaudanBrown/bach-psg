# bach-psg

`bach-psg` is an R `{targets}` pipeline for processing BACH polysomnography
(PSG) recordings, deriving spindle and slow-oscillation measures with Luna, and
running downstream association and moderation models against participant-level
phenotype data.

## Status

This repository is being cleaned up for public-facing use. The reproducible
pipeline lives in [_targets.R](./_targets.R) and the reusable functions under
[`R/`](./R). Raw EDF/XML files, local notes, and machine-specific artifacts are
intentionally excluded from version control.

## What The Pipeline Does

1. Reads EDF/XML PSG inputs and participant-level tabular data from locations
   provided by environment variables.
2. Uses Luna to derive empirical thresholds, spindle/SO summaries, and QC
   artifacts from BACH recordings.
3. Builds stage-specific derived datasets for N2 and N3 sleep.
4. Fits pre-specified regression and moderation models over merged PSG and
   phenotype data.
5. Writes selected outputs such as `psd_dt.csv` to the configured data
   directory.

## Repository Layout

- [`_targets.R`](./_targets.R): pipeline graph definition.
- [`R/`](./R): reusable pipeline functions, split by concern.
- [`docs/`](./docs): public-facing documentation, development guidance, and
  publication boundary notes.
- `data/`, `edfs/`, `Displayedsignals_inclFiltering/`, `_targets/`: local data
  and generated artifacts; these are not part of the public source tree.

## Getting Started

1. Enter the development shell with `nix develop`.
2. Copy [`.env.example`](./.env.example) to `.env` and set `EDF_DIR` and
   `DATA_DIR` for your local machine.
3. Confirm the expected input files exist under those directories.
4. Run `just manifest` to inspect the target graph.
5. Run `just run` to execute the pipeline.

## Environment

The pipeline expects:

- Luna and R dependencies available in the shell environment.
- `EDF_DIR`: directory containing PSG EDF files and matching `.XML` annotations.
- `DATA_DIR`: directory containing participant-level CSV inputs and receiving
  selected generated outputs.

See [docs/development.md](./docs/development.md) for local workflow details.

## Data And Privacy

This repository should be treated as code and reproducibility scaffolding, not a
distribution point for private or controlled participant data. See
[docs/data-boundary.md](./docs/data-boundary.md) for the intended publication
boundary.

## Scope

The tracked repository is intended to contain only the maintained pipeline, its
reusable functions, and the documentation needed to run or inspect that
pipeline. Legacy ad hoc scripts and serialized snapshot artifacts that are not
part of the current `{targets}` graph are intentionally removed from version
control.
