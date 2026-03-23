# Data Boundary

`bach-psg` is intended to publish code, workflow structure, and reproducibility
instructions without publishing private or controlled study data.

## Should Stay Out Of Version Control

- Raw EDF files.
- XML annotations accompanying participant PSG files.
- Local exports, zip archives, and generated cache directories.
- Scratch notes or machine-specific sync instructions.
- Environment files containing local paths or credentials.

## Can Be Documented Publicly

- The shape of expected inputs.
- Required environment variables and software dependencies.
- Pipeline stages and analysis intent.
- How to reproduce results when data access is approved separately.

## Current Working Rule

If a file contains participant-level raw data, locally generated derived data,
or machine-specific operational details, it should not be treated as a durable
public repository asset unless there is an explicit reason to publish it.
