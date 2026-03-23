# Artifact Verification

This repository currently has two distinct Luna behaviors that need to be kept
separate when reasoning about "clean" analysis data:

1. Artifact masking during EDF preprocessing.
2. Sleep-stage restriction during downstream analysis.

The current preprocessing write path in
[`R/edf_processing.R`](../R/edf_processing.R) runs:

```text
EPOCH
SIGNALS keep=${eeg}
ARTIFACTS
SIGSTATS
CHEP-MASK ep-th=3,3,3
CHEP epoch
DUMP-MASK annot=artifacts
WRITE-ANNOTS ...
WRITE ...
```

The stage-restricted analysis path later runs `MASK ifnot=<stage> & RE` on the
written EDF. That means stage exclusion is explicit, but artifact exclusion must
be verified separately.

## Verification Goal

Establish whether the written "filtered" EDF already excludes artifact-masked
epochs, or whether it only carries forward the data until an explicit artifact
`RE` is added.

## Reproducible Check

Use the verification helper on a valid EDF/XML pair:

```bash
just artifact-verify /absolute/path/to/BACH0001.edf N2
```

This compares:

- the current preprocessing write path
- an otherwise identical write path with explicit artifact `RE`

and reports, for both the full recording and the chosen stage-specific analysis
view:

- total epochs retained
- raw epochs retained after remapping via `E1`
- raw epochs retained that were flagged in raw QC
- dataset epochs that still appear masked after reload

## Interpretation

- If the current variant retains raw epochs that raw QC marked as masked, then
  artifact exclusion is not happening before downstream analysis.
- If the `with_re` variant removes those epochs and the current variant does
  not, then an explicit artifact `RE` is required to make the "cleaned" EDF
  actually exclude them.
- If both variants retain the same epoch set, then the current write path is
  already behaving like an excluded-epoch path and no behavior change is needed.

## Input Requirement

The verification requires a structurally valid EDF plus matching `.XML`
annotations. Clipped or partial EDF samples are not sufficient for this check,
because Luna will stop before the artifact pipeline runs.
