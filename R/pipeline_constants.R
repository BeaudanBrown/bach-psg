PIPELINE_DEFAULT_QC_COMMANDS <- c(
  "CHEP-MASK ep-th=3,3,3 max=200,0.05 clipped=0.05 flat=0.05"
)

PIPELINE_DEFAULT_EEG_CHANNELS <- c(
  "F3_M2",
  "F4_M1",
  "C3_M2",
  "C4_M1",
  "O1_M2",
  "O2_M1"
)

PIPELINE_FILTER_PROFILES <- list(
  base = list(
  ),
  bandpass_0_3_35 = list(
    filter_commands = "FILTER bandpass=0.3,35 ripple=0.02 tw=1"
  ),
  notch_50 = list(
    filter_commands = "FILTER bandstop=49,51 ripple=0.02 tw=1"
  ),
  bandpass_0_3_35_notch_50 = list(
    filter_commands = c(
      "FILTER bandpass=0.3,35 ripple=0.02 tw=1",
      "FILTER bandstop=49,51 ripple=0.02 tw=1"
    )
  )
)

PIPELINE_SLEEP_STAGES <- c("N2", "N3")
PIPELINE_SPINDLE_FREQS <- c(11, 15)
PIPELINE_CHANNEL_PREFIXES <- c("C3", "C4")
PIPELINE_OUTCOMES <- c("visualrepro2_total", "logicalmem_delay_total")
PIPELINE_PREDICTORS <- c("overlap", "angle", "mag")
PIPELINE_MODERATORS <- c(
  "ab4240ratio_plasma",
  "ptau217_mean_conc_plasma",
  "gfap_mean_conc_plasma"
)
PIPELINE_SPINDLE_CHANNELS <- c("C3_M2", "C4_M1", "F3_M2", "F4_M1")
PIPELINE_SPINDLE_THRESHOLD_CHANNELS <- c("C3_M2", "C4_M1")

PIPELINE_CHANNEL_EXCLUSIONS <- data.table::data.table(
  bach_id = c(
    "BACH0018",
    "BACH0243"
  ),
  drop_channels = list(
    c("C3_M2"),
    c("C4_M1")
  )
)
