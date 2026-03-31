library(luna)
library(data.table)

get_line_noise_review <- function(edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  xml_path <- paste0(edf_path, ".XML")

  ledf(edf_path, base_name, xml_path)
  leval("EPOCH & SIGNALS keep=${eeg}")
  psd <- leval("PSD spectrum peaks max=60")
  lrefresh()

  summary_dt <- extract_luna_table(
    psd,
    "CH",
    cols = c("CH", "KURT", "NE", "SPEC_SLOPE", "SPEC_SLOPE_MD", "SPEC_SLOPE_MN", "SPEC_SLOPE_SD", "SPK")
  )
  band_dt <- extract_first_available_table(
    psd,
    table_names = c("B_CH", "CH_F"),
    cols = NULL
  )

  if (nrow(summary_dt)) {
    summary_dt[, bach_id := base_name]
    setcolorder(summary_dt, c("bach_id", "CH"))
  }

  if (nrow(band_dt)) {
    band_dt[, bach_id := base_name]
  }

  list(
    summary = summary_dt,
    spectra = band_dt
  )
}
