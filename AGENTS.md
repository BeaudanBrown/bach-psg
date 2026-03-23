# AGENTS.md

## Luna Debugging

For Luna-related debugging in this project, do not try to infer command behavior by probing Luna directly first. Read the Luna documentation and use that as the primary source of truth before proposing or implementing changes.

Start here:
- `https://zzz.bwh.harvard.edu/luna/`

Most relevant sections for this pipeline:
- `https://zzz.bwh.harvard.edu/luna/ref/hypnograms/`
- `https://zzz.bwh.harvard.edu/luna/ref/artifacts/`
- `https://zzz.bwh.harvard.edu/luna/ref/masks/`
- `https://zzz.bwh.harvard.edu/luna/ref/spindles-so/`
- `https://zzz.bwh.harvard.edu/luna/ref/epochs/`
- `https://zzz.bwh.harvard.edu/luna/ref/annotations/`

Specific guidance:
- Use the docs to confirm what tables and variables commands emit before changing extraction code.
- For epoch-level sleep stage labels, check the `STAGE` and `HYPNO` documentation rather than assuming output shapes.
- The hypnogram docs note that these commands require staging annotations to be present.
- The docs also note that `STAGE` is the command that exports sleep stage information per epoch, whereas `HYPNO` is primarily for hypnogram summaries and related derived outputs.

If a Luna behavior still appears inconsistent after checking the docs, document the exact page consulted and the specific command/output expectation before adding code changes.
