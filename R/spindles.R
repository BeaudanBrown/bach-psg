library(luna)
library(data.table)
library(dplyr)

process_edf <- function(filtered_dir, edf_path) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  result <- data.table(
    bach_id = base_name
  )

  filter_and_load_edf(filtered_dir, edf_path, base_name)

  # N2 and N3 combined
  leval("MASK ifnot=N2,N3 & RE")
  result$psd <- leval("PSD spectrum")
  result$spindles <- leval(
    "SPINDLES fc=11,15 empirical set-empirical median"
  )
  lrefresh()
  result
}

extract_raw_data <- function(filtered_dir, valid_pairs) {
  ppt_results <- list()
  for (edf_path in valid_pairs) {
    base_name <- tools::file_path_sans_ext(basename(edf_path))
    ppt_results[[base_name]] <- list()

    filter_and_load_edf(filtered_dir, edf_path, base_name)

    # N2
    leval("MASK ifnot=N2 & RE")
    ppt_results[[base_name]][["N2"]]$psd <- leval("PSD spectrum")
    ppt_results[[base_name]][["N2"]]$spindles <- leval(
      "SPINDLES fc=11,15 empirical set-empirical median"
    )
    lrefresh()

    # N3
    leval("MASK ifnot=N3 & RE")
    ppt_results[[base_name]][["N3"]]$psd <- leval("PSD spectrum")
    ppt_results[[base_name]][["N3"]]$spindles <- leval(
      "SPINDLES fc=11,15 empirical set-empirical median"
    )
    lrefresh()
  }
  ppt_results
}

filter_and_load_edf <- function(filtered_dir, edf_path, base_name) {
  xml_path <- paste0(edf_path, ".XML")
  filtered_path <- file.path(filtered_dir, paste0(base_name, ".edf"))

  if (!file.exists(filtered_path)) {
    ledf(edf_path, base_name, xml_path)
    leval(sprintf(
      "EPOCH &
      SIGNALS keep=${eeg} &
      ARTIFACTS &
      SIGSTATS &
      CHEP-MASK ep-th=3,3,3 &
      CHEP epoch &
      DUMP-MASK annot=artifacts &
      WRITE-ANNOTS file=%s/%s.annots &
      WRITE edf-dir=%s",
      filtered_dir,
      base_name,
      filtered_dir
    ))
  }

  ledf(filtered_path, base_name, xml_path)
}

get_empirical_threshold <- function(ppt_results, channels = c("C3_M2", "C4_M1", "F3_M2", "F4_M1"))
{
  CH_F <- data.table(ppt_results$spindles[[1]]$CH_F)
  CH_F <- CH_F[CH %in% channels, ]
  median(CH_F$EMPTH)
}

get_spindles_with_threshold <- function(filtered_dir, edf_path, threshold) {
  base_name <- tools::file_path_sans_ext(basename(edf_path))
  result <- data.table(
    bach_id = base_name
  )
  
  filter_and_load_edf(filtered_dir, edf_path, base_name)
  
  # N2 and N3 combined
  leval("MASK ifnot=N2,N3 & RE")
  result$psd <- leval("PSD spectrum")
  result$spindles <- leval(
    paste0("SPINDLES fc=11,15 th=", threshold, " so f-lwr=0.3 f-upr=4 t-neg-lwr=0.3 t-neg-upr=1.5 t-pos-lwr=0 t-pos-upr=1.0 uV-neg=-40 uV-p2p=75 nreps=100000")
  )
  lrefresh()
  result
}
  
# code added by Abby
filter_spindles_so <- function(threshold_results) {
  filtered_spindles <- data.table(threshold_results$spindles[[1]]$CH_F)
  filtered_spindles <- filtered_spindles[, c("ID", "CH", "F", "N", "AMP", "CHIRP",
                                              "DENS", "DUR", "COUPL_MAG",
                                              "COUPL_OVERLAP", "COUPL_ANGLE",
                                              "COUPL_OVERLAP_EMP", "COUPL_MAG_EMP",
                                              "COUPL_OVERLAP_Z", "COUPL_MAG_Z", "Q")]
                                             "DENS", "DUR", "COUPL_MAG",
                                             "COUPL_OVERLAP", "COUPL_ANGLE",
                                             "COUPL_OVERLAP_EMP", "COUPL_MAG_EMP",
                                             "COUPL_OVERLAP_Z", "COUPL_MAG_Z", "Q")]
  
  filtered_so <- data.table(threshold_results$spindles[[1]]$CH)
  filtered_so <- filtered_so[, c("ID", "CH", "SO", "SO_RATE", "SO_AMP_NEG", "SO_AMP_POS", "SO_AMP_P2P", "SO_DUR")]
  filtered_so <- filtered_so[, c("ID", "CH", "SO", "SO_RATE", "SO_NEG_AMP", "SO_POS_AMP", "SO_P2P", "SO_DUR")]
  
  merged_data <- merge(filtered_spindles, filtered_so, by = c("ID", "CH"), all = TRUE)
  
  return(data.table(merged_data)
  )
}

# Make COUPL_ANGLE NA if COUPL_MAG_EMP > 0.05, ADD flag where true = coupl_mag_emp > 0.05
clean_angle <- function(filtered_results) {
  cleaned_angle <- copy(filtered_results)
  cleaned_angle[, COUPL_ANGLE_EXCLUDED := COUPL_MAG_EMP > 0.05]
  cleaned_angle[COUPLE_ANGLE_EXCLUDED == TRUE, COUPL_ANGLE := NA]
  
  return(clean_angle)
}
 


#reattempt <- extract_raw_data(valid_pairs)
#saverds(reattempt, "reattempt_thresh.rds")

## once this has ran successfully, you can load the values without having to redo like this
#reattempt_thresh <- readrds("reattempt_thresh.rds")
#first_thresh <- readrds("results2.rds")

## edit: filtered out occipital channels
#process_results <- function(first_thresh) {
#  # todo: extract the relevant values from all the outputs
#  spindle_thresholds_one <- rbindlist(lapply(names(first_thresh), function(id) {
    
#    n2_thrsh <- first_thresh[[id]]$n2$spindles$spindles$ch_f %>%
#      dplyr::filter(ch %in% c("f3_m2", "f4_m1", "c3_m2", "c4_m1")) %>%
#      data.table()
#      n2_thrsh[, participant := id]
#      n2_thrsh[, stage := "n2"]
      
#    n3_thrsh <- first_thresh[[id]]$n3$spindles$spindles$ch_f %>%
#      dplyr::filter(ch %in% c("f3_m2", "f4_m1", "c3_m2", "c4_m1")) %>%
#      data.table()
#      n3_thrsh[, participant := id]
#      n3_thrsh[, stage := "n3"]
    
    
#   rbind(n2_thrsh, n3_thrsh)
    
#  }))
#  return(spindle_thresholds_one)
#}
#spindle_thresholds_one <- process_results(first_thresh)

##new
#write.csv(spindle_thresh_na, "spindle_thresh_na.csv", row.names = false)
  
## average results to find mean spindle threshold ## edit: changed to media
##median(results2$n2_median_spindle_thrsh)
##median(results2$n3_median_spindle_thrsh)

#median(less.than.20$empth) 8.25 
#mean(less.than.20$empth, na.rm = true)

#thresh_exl <- spindle_thresh_reattempt[empth >= 20, .(id, ch, empth, f, stage, n)]
#length(unique(thresh_exl$id)) $74

#thresh_exl_one <- spindle_thresholds_one[empth >= 20, .(id, ch, empth, f, stage, n)]
#length(unique(thresh_exl_one$id)) 

#first_thresh[empth >= 20, .n]

#less.than.20 <- spindle_thresh_reattempt[empth <= 20, ]
#length(unique(spindle_thresh_reattempt$id)) #139

#greater.than.ten <- spindle_thresh_na[n >= 10]

#median(greater.than.ten$empth)
#mean(greater.than.ten$empth)

#mean(results2$n2_mean_spindle_thrsh + results2$n3_mean_spindle_thrsh)

#total <- sum(results2$n2_mean_spindle_thrsh, results2$n3_mean_spindle_thrsh)











# Average = 9.72?
  #N2_PSD_chf <- as.data.table(raw$N2$psd$PSD$CH_F)

# Semi-cursed chatgpt code below
# ===================================================================================

# # Loop over each valid EDF-XML pair

# # Combine all spindles into a single data.table
# n2_spindles_dt <- rbindlist(results$spindles_n2, use.names = TRUE, fill = TRUE)
# n3_spindles_dt <- rbindlist(results$spindles_n3, use.names = TRUE, fill = TRUE)

# # Combine all so into a single data.table
# n2_so_dt <- rbindlist(results$so_list_n2, use.names = TRUE, fill = TRUE)
# n3_so_dt <- rbindlist(results$so_list_n3, use.names = TRUE, fill = TRUE)

# # Combine all PSD for n2 into a data.table
# PSD_dt_n2_chf <- rbindlist(
#   results$psd_chf_list_n2,
#   use.names = TRUE,
#   fill = TRUE
# )
# PSD_dt_n2_bch <- rbindlist(
#   results$psd_bch_list_n2,
#   use.names = TRUE,
#   fill = TRUE
# )

# # Combine all PSD for n3 into a data.table
# PSD_dt_n3_chf <- rbindlist(
#   results$psd_chf_list_n3,
#   use.names = TRUE,
#   fill = TRUE
# )
# PSD_dt_n3_bch <- rbindlist(
#   results$psd_bch_list_n3,
#   use.names = TRUE,
#   fill = TRUE
# )

# # Save all as csv
# save_path_csv <- "/data/eeg-bach/Results/n2_spindles_dt_t4.5.csv"
# fwrite(n2_spindles_dt_e, file = save_path_csv)

# save_path_csv <- "/data/eeg-bach/Results/n3_spindles_dt_t4.5.csv"
# fwrite(n3_spindles_dt_e, file = save_path_csv)

# save_path_csv <- "/data/eeg-bach/Results/n2_so_dt_t4.5.csv"
# fwrite(n2_so_dt_e, file = save_path_csv)

# save_path_csv <- "/data/eeg-bach/Results/n3_so_dt_t4.5.csv"
# fwrite(n3_so_dt_e, file = save_path_csv)

# save_path_csv <- "/data/eeg-bach/Results/n2_PSD_chf_dt_t4.5.csv"
# fwrite(PSD_dt_n2_chf_e, file = save_path_csv)

# save_path_csv <- "/data/eeg-bach/Results/n2_PSD_bch_dt_t4.5.csv"
# fwrite(PSD_dt_n2_bch_e, file = save_path_csv)

# save_path_csv <- "/data/eeg-bach/Results/n3_PSD_chf_dt_t4.5.csv"
# fwrite(PSD_dt_n3_chf_e, file = save_path_csv)

# save_path_csv <- "/data/eeg-bach/Results/n3_PSD_bch_dt_t4.5.csv"
# fwrite(PSD_dt_n3_bch_e, file = save_path_csv)
