PIPELINE_ECG_CHANNEL <- "EKG"
PIPELINE_EEG_CHANNELS <- c("C3_M2", "C4_M1")
PIPELINE_EEG_SIGNAL_LIST <- paste(PIPELINE_EEG_CHANNELS, collapse = ",")

PIPELINE_FILTER_PROFILES <- list(
  unfiltered = list(
    commands = c(
      "EPOCH"
    )
  ),
  original = list(
    commands = c(
      "EPOCH",
      sprintf("SUPPRESS-ECG ecg=%s", PIPELINE_ECG_CHANNEL),
      sprintf("EDGER sig=%s all epoch mask", PIPELINE_EEG_SIGNAL_LIST),
      sprintf("ARTIFACTS sig=%s", PIPELINE_EEG_SIGNAL_LIST),
      sprintf("SIGSTATS sig=%s epoch", PIPELINE_EEG_SIGNAL_LIST),
      sprintf("CHEP-MASK sig=%s ep-th=3,3,3", PIPELINE_EEG_SIGNAL_LIST),
      "CHEP epoch",
      "DUMP-MASK annot=artifacts"
    )
  ),
  base = list(
    commands = c(
      "EPOCH",
      sprintf("SUPPRESS-ECG ecg=%s", PIPELINE_ECG_CHANNEL),
      sprintf("EDGER sig=%s all epoch mask", PIPELINE_EEG_SIGNAL_LIST),
      sprintf("ARTIFACTS sig=%s", PIPELINE_EEG_SIGNAL_LIST),
      sprintf("SIGSTATS sig=%s epoch", PIPELINE_EEG_SIGNAL_LIST),
      sprintf(
        "CHEP-MASK sig=%s ep-th=3,3,3 max=200,0.05 clipped=0.05 flat=0.05",
        PIPELINE_EEG_SIGNAL_LIST
      ),
      "CHEP epoch",
      "DUMP-MASK annot=artifacts"
    )
  ),
  bandpass_0_3_35 = list(
    commands = c(
      "EPOCH",
      sprintf("SUPPRESS-ECG ecg=%s", PIPELINE_ECG_CHANNEL),
      sprintf("EDGER sig=%s all epoch mask", PIPELINE_EEG_SIGNAL_LIST),
      "FILTER bandpass=0.3,35 ripple=0.02 tw=1",
      sprintf("ARTIFACTS sig=%s", PIPELINE_EEG_SIGNAL_LIST),
      sprintf("SIGSTATS sig=%s epoch", PIPELINE_EEG_SIGNAL_LIST),
      sprintf(
        "CHEP-MASK sig=%s ep-th=3,3,3 max=200,0.05 clipped=0.05 flat=0.05",
        PIPELINE_EEG_SIGNAL_LIST
      ),
      "CHEP epoch",
      "DUMP-MASK annot=artifacts"
    )
  ),
  notch_50 = list(
    commands = c(
      "EPOCH",
      sprintf("SUPPRESS-ECG ecg=%s", PIPELINE_ECG_CHANNEL),
      sprintf("EDGER sig=%s all epoch mask", PIPELINE_EEG_SIGNAL_LIST),
      "FILTER bandstop=49,51 ripple=0.02 tw=1",
      sprintf("ARTIFACTS sig=%s", PIPELINE_EEG_SIGNAL_LIST),
      sprintf("SIGSTATS sig=%s epoch", PIPELINE_EEG_SIGNAL_LIST),
      sprintf(
        "CHEP-MASK sig=%s ep-th=3,3,3 max=200,0.05 clipped=0.05 flat=0.05",
        PIPELINE_EEG_SIGNAL_LIST
      ),
      "CHEP epoch",
      "DUMP-MASK annot=artifacts"
    )
  )
)

PIPELINE_SLEEP_STAGES <- c("N2", "N3")
PIPELINE_SPINDLE_FREQS <- c(11, 15)
PIPELINE_CHANNEL_PREFIXES <- c("C3", "C4")
PIPELINE_MANDATORY_KEEP_CHANNELS <- c(PIPELINE_ECG_CHANNEL)
PIPELINE_DEFAULT_KEEP_CHANNELS <- c(PIPELINE_EEG_CHANNELS, PIPELINE_MANDATORY_KEEP_CHANNELS)
PIPELINE_OUTCOMES <- c("visualrepro2_total", "logicalmem_delay_total")
PIPELINE_PREDICTORS <- c("overlap", "angle", "mag")
PIPELINE_MODERATORS <- c(
  "ab4240ratio_plasma",
  "ptau217_mean_conc_plasma",
  "gfap_mean_conc_plasma"
)

PIPELINE_CHANNEL_KEEP_OVERRIDES <- list(
  BACH0018 = c("C4_M1"),
  BACH0022 = c("C4_M1"),
  BACH0026 = c("C4_M1"),
  BACH0052 = c("C3_M2"),
  BACH0072 = c("C3_M2"),
  BACH0088 = c("C4_M1"),
  BACH0166 = c("C4_M1"),
  BACH0184 = c("C3_M2"),
  BACH0188 = c("C3_M2"),
  BACH0197 = c("C3_M2"),
  BACH0201 = c("C3_M2"),
  BACH0224 = c("C3_M2"),
  BACH0232 = c("C4_M1"),
  BACH0243 = c("C3_M2"),
  BACH0259 = c(),
  BACH0254 = c("C4_M1")
)
