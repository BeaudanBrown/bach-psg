PIPELINE_FILTER_PROFILES <- list(
  unfiltered = list(
    commands = c(
      "EPOCH"
    )
  ),
  original = list(
    commands = c(
      "EPOCH",
      "SUPPRESS-ECG ecg=ECG",
      "EDGER sig=* epoch mask",
      "ARTIFACTS",
      "SIGSTATS",
      "CHEP-MASK ep-th=3,3,3",
      "CHEP epoch",
      "DUMP-MASK annot=artifacts"
    )
  ),
  base = list(
    commands = c(
      "EPOCH",
      "SUPPRESS-ECG ecg=ECG",
      "EDGER sig=* epoch mask",
      "ARTIFACTS",
      "SIGSTATS",
      "CHEP-MASK ep-th=3,3,3 max=200,0.05 clipped=0.05 flat=0.05",
      "CHEP epoch",
      "DUMP-MASK annot=artifacts"
    )
  ),
  bandpass_0_3_35 = list(
    commands = c(
      "EPOCH",
      "SUPPRESS-ECG ecg=ECG",
      "EDGER sig=* epoch mask",
      "FILTER bandpass=0.3,35 ripple=0.02 tw=1",
      "ARTIFACTS",
      "SIGSTATS",
      "CHEP-MASK ep-th=3,3,3 max=200,0.05 clipped=0.05 flat=0.05",
      "CHEP epoch",
      "DUMP-MASK annot=artifacts"
    )
  ),
  notch_50 = list(
    commands = c(
      "EPOCH",
      "SUPPRESS-ECG ecg=ECG",
      "EDGER sig=* epoch mask",
      "FILTER bandstop=49,51 ripple=0.02 tw=1",
      "ARTIFACTS",
      "SIGSTATS",
      "CHEP-MASK ep-th=3,3,3 max=200,0.05 clipped=0.05 flat=0.05",
      "CHEP epoch",
      "DUMP-MASK annot=artifacts"
    )
  )
)

PIPELINE_SLEEP_STAGES <- c("N2", "N3")
PIPELINE_SPINDLE_FREQS <- c(11, 15)
PIPELINE_CHANNEL_PREFIXES <- c("C3", "C4")
PIPELINE_DEFAULT_KEEP_CHANNELS <- c("C3_M2", "C4_M1")
PIPELINE_OUTCOMES <- c("visualrepro2_total", "logicalmem_delay_total")
PIPELINE_PREDICTORS <- c("overlap", "angle", "mag")
PIPELINE_MODERATORS <- c(
  "ab4240ratio_plasma",
  "ptau217_mean_conc_plasma",
  "gfap_mean_conc_plasma"
)

PIPELINE_CHANNEL_KEEP_OVERRIDES <- list(
  BACH0018 = c("C4_M1"),
  BACH0243 = c("C3_M2"),
  BACH0052 = c("C3_M2")
)
