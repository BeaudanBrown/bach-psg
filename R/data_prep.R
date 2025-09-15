

## read in csv as data.table
#dc <- fread("data_cleaner_output.csv")
#visualmem <- fread("data_cleaner_visualmem.csv")
##empth <- fread("spindle_empth_thresh.csv")
#new_data <- fread("spindle_7.8.csv")
#new_ppts <- fread("spindle_new.ppt.csv")

##adding new ppts
#new_data_m <- rbind(new_data, new_ppts, fill = TRUE)
#length(unique(new_data_m$ID)) # 2044 observations, 138 ppts

## seeing how many ppts
## Filter rows that meet the criteria and select relevant columns
##slow_nonsig <- new_data_m[F == 11 & COUPL_OVERLAP_EMP > 0.05 & COUPL_MAG_EMP > 0.05, .(ID, CH, F, COUPL_OVERLAP_EMP, COUPL_MAG_EMP)]
##length(unique(slow_nonsig$ID))

##slow_sig <-  new_data_m[F == 11 & COUPL_OVERLAP_EMP <= 0.05 & COUPL_MAG_EMP <= 0.05, .(ID, CH, F, COUPL_OVERLAP_EMP, COUPL_MAG_EMP)]


## IDs where one variable is <=0.05 and the other >0.05
##mixed <- new_data_m[F == 11 & ((COUPL_OVERLAP_EMP <= 0.05 & COUPL_MAG_EMP > 0.05) |
##(COUPL_OVERLAP_EMP > 0.05 & COUPL_MAG_EMP <= 0.05)), .(ID, CH, F, COUPL_OVERLAP_EMP, COUPL_MAG_EMP)]
##length(unique(mixed$ID)) #118

## making predictors vars NA if emprical value below signifcance

##emp_NA <- new_data_m[COUPL_OVERLAP_EMP > 0.05 | COUPL_MAG_EMP > 0.05, 
#                        #`:=`(COUPL_ANGLE = NA, COUPL_MAG = NA, COUPL_OVERLAP = NA)]

#emp_NA <- new_data_m[
#  , ':='(
#    excluded = (COUPL_OVERLAP_EMP > 0.05 | COUPL_MAG_EMP > 0.05) %in% TRUE,
#    COUPL_ANGLE = fifelse(COUPL_OVERLAP_EMP > 0.05 | COUPL_MAG_EMP > 0.05, NA, COUPL_ANGLE),
#    COUPL_MAG = fifelse(COUPL_OVERLAP_EMP > 0.05 | COUPL_MAG_EMP > 0.05, NA, COUPL_MAG),
#    COUPL_OVERLAP = fifelse(COUPL_OVERLAP_EMP > 0.05 | COUPL_MAG_EMP > 0.05, NA, COUPL_OVERLAP)
#  )
#]

#emp_NA[, sum(excluded)] # 1751 excluded
#emp_NA[!is.na(COUPL_ANGLE), uniqueN(ID)] # 88 unique ppts

## empricical cleaning
##new_data_emp<- new_data_m[COUPL_OVERLAP_EMP <= 0.05 & COUPL_MAG_EMP <= 0.05] 
##length(unique(new_data_emp$ID)) # 273 observations, 88 ppts


## subset data cleaner output for only necessary variables
#dc_sub <- dc[ , .(subject_id, apoe_e4_status, ab4240ratio_plasma,
#                  psg_ahi_total_nrem, psg_tst, psg_waso,
#                  ptau217_cv_plasma, ptau217_mean_conc_plasma,
#                  ptau181_cv_plasma, ptau181_mean_conc_plasma,
#                  gfap_cv_plasma, gfap_mean_conc_plasma,
#                  ab42_cv_plasma, ab42_mean_conc_plasma,
#                  ab40_cv_plasma, ab40_mean_conc_plasma,
#                  demreview_status, logicalmem_delay_total,
#                  logicalmem_delay_storyc, logicalmem_delay_storyb,
#                  logicalmem_imm_total, logicalmem_imm_storyc,
#                  logicalmem_imm_storyb, education_highest, 
#                  education, sex, age)]

## adjusting id names 
#visualmem <- visualmem[, .(subject_id, visualrepro1_total, visualrepro2_total)]

##merge dc and visual
#dc_sub <- merge(dc_sub, visualmem, by = "subject_id", all.x = TRUE)

## adjust ID names in dc_sub
#dc_sub[, sid_num := as.numeric(subject_id)]
#dc_sub[, subject_id := fifelse(sid_num < 10, sprintf("BACH000%d", sid_num),
#                               fifelse(sid_num < 100, sprintf("BACH00%d", sid_num),
#                                       sprintf("BACH0%d", sid_num)))]
#dc_sub[, sid_num := NULL]
#as.character(dc_sub$subject_id)
#setnames(dc_sub, "subject_id", "ID")

#dc_sub[subject_id]

## dummy code sex 
#dc_sub[, sex := factor(
#  sex, levels = c("Male","Female"), 
#  labels = c(0, 1))]

## dummy code apoe4 status
#dc_sub[, apoe_e4_status := factor(
#  apoe_e4_status, levels = c("noncarrier", "carrier"),
#  labels = c(0, 1))]

## mean centering education 
#dc_sub[, education_centered := education - mean(education, na.rm = TRUE)]


## write cleaned data cleaner to csv 
#write.csv(dc_sub, "data_cleaner.v2.csv", row.names = FALSE)

## subset for slow and fast spindles
#fast <- emp_NA[F == 15]
#slow <- emp_NA[F == 11]


## Step 1: Keep unique combinations of ID, stage, channel, and frequency
##present <- unique(new_data_m[, .(ID, stage, CH, F)])

## Step 2: Pivot to wide format: one row per ID × stage × channel, columns for F = 11 and F = 15
##wide_freq <- dcast(present, ID + stage + CH ~ F, fun.aggregate = length, value.var = "F")

## Step 3: Rename columns for clarity
##setnames(wide_freq, old = c("11", "15"), new = c("F11_present", "F15_present"))

## Step 4: Convert counts to TRUE/FALSE
##wide_freq[, `:=`(
#  #F11_present = F11_present > 0,
#  #F15_present = F15_present > 0
##)]

## Step 5: Filter rows missing either frequency
##missing_freq <- wide_freq[!F11_present | !F15_present]

## View the missing combinations
##missing_freq

## fill in missing frequencies creating full dt will all combos of stagexchannelxF

## Step 1: Identify all unique IDs, stages, and channels
#all_IDs <- unique(new_data_m$ID)
#all_stages <- unique(new_data_m$stage)
#all_CHs <- unique(new_data_m$CH)
#all_Fs <- c(11, 15)

## Step 2: Create a complete grid of all combinations
#full_grid <- CJ(ID = all_IDs, stage = all_stages, CH = all_CHs, F = all_Fs, unique = TRUE)

## Step 3: Left join your existing data onto the full grid
#new_data_m_complete <- merge(full_grid, new_data_m,
#                             by = c("ID", "stage", "CH", "F"),
#                             all.x = TRUE,
#                             sort = FALSE)

## add a flag column indicating which rows were added
#new_data_m_complete[, missing_flag := is.na(N)] # missing = TRUE, row was added

## subset for fast and slow spindles 
#fast_comp <- new_data_m_complete[F == 15] # 1104, 138 ppts 51 ppts without NA
#slow_comp <- new_data_m_complete[F == 11] #1104, 138 ppts, 75 ppts without NA

## subset for sleep stage
#N2_fast <- fast_comp[stage == "N2"] # 138 ppts, 38 ppts without NA
#N2_slow <- slow_comp[stage == "N2"] # 66 ppts without NA

#N3_fast <- fast_comp[stage == "N3"] # 32 ppts without NA
#N3_slow <- slow_comp[stage == "N3"] # 36 ppts without NA

## average by stage
#fast_avg <- fast_comp[, lapply(.SD, mean, na.rm = TRUE),
#                 by = .(ID, CH),
#                 .SDcols = c("N", "AMP", "CHIRP", "DENS", "DUR",
#                             "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE",
#                             "COUPL_OVERLAP_EMP", "COUPL_MAG_EMP",
#                             "Q", "SO", "SO_RATE", "SO_AMP_NEG",
#                             "SO_AMP_POS", "SO_AMP_P2P", "SO_DUR")]

#slow_avg <- slow_comp[, lapply(.SD, mean, na.rm = TRUE),
#                 by = .(ID, CH),
#                 .SDcols = c("N", "AMP", "CHIRP", "DENS", "DUR",
#                             "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE",
#                             "COUPL_OVERLAP_EMP", "COUPL_MAG_EMP",
#                             "Q", "SO", "SO_RATE", "SO_AMP_NEG",
#                             "SO_AMP_POS", "SO_AMP_P2P", "SO_DUR")]

#overall_avg <- new_data_m_complete[, lapply(.SD, mean, na.rm = TRUE),
#                            by = .(ID, CH),
#                            .SDcols = c("N", "AMP", "CHIRP", "DENS", "DUR",
#                                        "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE",
#                                        "COUPL_OVERLAP_EMP", "COUPL_MAG_EMP",
#                                        "Q", "SO", "SO_RATE", "SO_AMP_NEG",
#                                        "SO_AMP_POS", "SO_AMP_P2P", "SO_DUR")] 
## overall_avg = 88ppts

## convert to wide format --------------------------------------------------


#Fast.w <- dcast(
#  fast_avg, 
#  ID ~ CH,
#  value.var = c("N", "CHIRP", "DENS", "DUR",
#                "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE", "COUPL_OVERLAP_EMP",
#                "COUPL_MAG_EMP", "Q", "SO", "SO_RATE", "SO_AMP_NEG", "SO_AMP_POS",
#                "SO_AMP_P2P", "SO_DUR"))
#Slow.w <- dcast(
#  slow_avg, 
#  ID ~ CH,
#  value.var = c("N", "CHIRP", "DENS", "DUR",
#                "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE", "COUPL_OVERLAP_EMP",
#                "COUPL_MAG_EMP", "Q", "SO", "SO_RATE", "SO_AMP_NEG", "SO_AMP_POS",
#                "SO_AMP_P2P", "SO_DUR"))

#N2_fast.w <- dcast(
#  N2_fast, 
#  ID ~ CH,
#  value.var = c("N", "CHIRP", "DENS", "DUR",
#                "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE", "COUPL_OVERLAP_EMP",
#                "COUPL_MAG_EMP", "Q", "SO", "SO_RATE", "SO_AMP_NEG", "SO_AMP_POS",
#                "SO_AMP_P2P", "SO_DUR"))

#N2_slow.w <- dcast(
#  N2_slow, 
#  ID ~ CH,
#  value.var = c("N", "CHIRP", "DENS", "DUR",
#                "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE", "COUPL_OVERLAP_EMP",
#                "COUPL_MAG_EMP", "Q", "SO", "SO_RATE", "SO_AMP_NEG", "SO_AMP_POS",
#                "SO_AMP_P2P", "SO_DUR"))

#N3_fast.w <- dcast(
#  N3_fast, 
#  ID ~ CH,
#  value.var = c("N", "CHIRP", "DENS", "DUR",
#                "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE", "COUPL_OVERLAP_EMP",
#                "COUPL_MAG_EMP", "Q", "SO", "SO_RATE", "SO_AMP_NEG", "SO_AMP_POS",
#                "SO_AMP_P2P", "SO_DUR"))

#N3_slow.w <- dcast(
#  N3_slow, 
#  ID ~ CH,
#  value.var = c("N", "CHIRP", "DENS", "DUR",
#                "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE", "COUPL_OVERLAP_EMP",
#                "COUPL_MAG_EMP", "Q", "SO", "SO_RATE", "SO_AMP_NEG", "SO_AMP_POS",
#                "SO_AMP_P2P", "SO_DUR"))

#overall_avg.w <- dcast(
#  overall_avg, 
#  ID ~ CH,
#  value.var = c("N", "CHIRP", "DENS", "DUR",
#                "COUPL_MAG", "COUPL_OVERLAP", "COUPL_ANGLE", "COUPL_OVERLAP_EMP",
#                "COUPL_MAG_EMP", "Q", "SO", "SO_RATE", "SO_AMP_NEG", "SO_AMP_POS",
#                "SO_AMP_P2P", "SO_DUR"))


## Merging -----------------------------------------------------------------


#Fast.dt <- merge(Fast.w, dc_sub, by = "ID", all.x = TRUE)
#Slow.dt <- merge(Slow.w, dc_sub, by = "ID", all.x = TRUE)

#N2_fast.dt <- merge(N2_fast.w, dc_sub, by = "ID", all.x = TRUE)
#N2_slow.dt <- merge(N2_slow.w, dc_sub, by = "ID", all.x = TRUE)

#N3_fast.dt <- merge(N3_fast.w, dc_sub, by = "ID", all.x = TRUE)
#N3_slow.dt <- merge(N3_slow.w, dc_sub, by = "ID", all.x = TRUE)

#alldt <- merge(overall_avg.w, dc_sub, by = "ID", all.x = TRUE)


## Averaging ---------------------------------------------------------------


## full dt
#alldt[, couple_angle_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_ANGLE_C3_M2", "COUPL_ANGLE_C4_M1", 
#                    "COUPL_ANGLE_F3_M2", "COUPL_ANGLE_F4_M1")]

#alldt[, couple_overlap_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_OVERLAP_C3_M2", "COUPL_OVERLAP_C4_M1", 
#                    "COUPL_OVERLAP_F3_M2", "COUPL_OVERLAP_F4_M1")]

#alldt[, couple_mag_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_MAG_C3_M2", "COUPL_MAG_C4_M1", 
#                    "COUPL_MAG_F3_M2", "COUPL_MAG_F4_M1")]

## all dt frontal 
#alldt <- alldt %>%
#  mutate(
#    COUPL_OVERLAP_frontal = rowMeans(across(matches("^COUPL_OVERLAP_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_frontal     = rowMeans(across(matches("^COUPL_MAG_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_frontal   = rowMeans(across(matches("^COUPL_ANGLE_F[34]_M[12]$")), na.rm = TRUE)
#  )

## all dt central
#alldt <- alldt %>%
#  mutate(
#    COUPL_OVERLAP_central = rowMeans(across(matches("^COUPL_OVERLAP_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_central     = rowMeans(across(matches("^COUPL_MAG_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_central   = rowMeans(across(matches("^COUPL_ANGLE_C[34]_M[12]$")), na.rm = TRUE)
#  )

## averaging for overall fast
#Fast.dt[, couple_angle_mean := rowMeans(.SD, na.rm = TRUE),
#      .SDcols = c("COUPL_ANGLE_C3_M2", "COUPL_ANGLE_C4_M1", 
#                  "COUPL_ANGLE_F3_M2", "COUPL_ANGLE_F4_M1")]

#Fast.dt[, couple_overlap_mean := rowMeans(.SD, na.rm = TRUE),
#      .SDcols = c("COUPL_OVERLAP_C3_M2", "COUPL_OVERLAP_C4_M1", 
#                  "COUPL_OVERLAP_F3_M2", "COUPL_OVERLAP_F4_M1")]

#Fast.dt[, couple_mag_mean := rowMeans(.SD, na.rm = TRUE),
#      .SDcols = c("COUPL_MAG_C3_M2", "COUPL_MAG_C4_M1", 
#                  "COUPL_MAG_F3_M2", "COUPL_MAG_F4_M1")]
## fast frontal
#Fast.dt <- Fast.dt %>%
#  mutate(
#    COUPL_OVERLAP_frontal = rowMeans(across(matches("^COUPL_OVERLAP_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_frontal     = rowMeans(across(matches("^COUPL_MAG_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_frontal   = rowMeans(across(matches("^COUPL_ANGLE_F[34]_M[12]$")), na.rm = TRUE)
#  )

## fast central
#Fast.dt <- Fast.dt %>%
#  mutate(
#    COUPL_OVERLAP_central = rowMeans(across(matches("^COUPL_OVERLAP_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_central     = rowMeans(across(matches("^COUPL_MAG_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_central   = rowMeans(across(matches("^COUPL_ANGLE_C[34]_M[12]$")), na.rm = TRUE)
#  )

## Averaging N2 fast 
#N2_fast.dt[, couple_angle_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_ANGLE_C3_M2", "COUPL_ANGLE_C4_M1", 
#                    "COUPL_ANGLE_F3_M2", "COUPL_ANGLE_F4_M1")]

#N2_fast.dt[, couple_overlap_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_OVERLAP_C3_M2", "COUPL_OVERLAP_C4_M1", 
#                    "COUPL_OVERLAP_F3_M2", "COUPL_OVERLAP_F4_M1")]

#N2_fast.dt[, couple_mag_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_MAG_C3_M2", "COUPL_MAG_C4_M1", 
#                    "COUPL_MAG_F3_M2", "COUPL_MAG_F4_M1")]

## N2 fast frontal
#N2_fast.dt <- N2_fast.dt %>%
#  mutate(
#    COUPL_OVERLAP_frontal = rowMeans(across(matches("^COUPL_OVERLAP_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_frontal     = rowMeans(across(matches("^COUPL_MAG_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_frontal   = rowMeans(across(matches("^COUPL_ANGLE_F[34]_M[12]$")), na.rm = TRUE)
#  )

## N2 fast central
#N2_fast.dt <- N2_fast.dt %>%
#  mutate(
#    COUPL_OVERLAP_central = rowMeans(across(matches("^COUPL_OVERLAP_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_central     = rowMeans(across(matches("^COUPL_MAG_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_central   = rowMeans(across(matches("^COUPL_ANGLE_C[34]_M[12]$")), na.rm = TRUE)
#  )

## averaging fast N3
#N3_fast.dt[, couple_angle_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_ANGLE_C3_M2", "COUPL_ANGLE_C4_M1", 
#                       "COUPL_ANGLE_F3_M2", "COUPL_ANGLE_F4_M1")]

#N3_fast.dt[, couple_overlap_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_OVERLAP_C3_M2", "COUPL_OVERLAP_C4_M1", 
#                       "COUPL_OVERLAP_F3_M2", "COUPL_OVERLAP_F4_M1")]

#N3_fast.dt[, couple_mag_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_MAG_C3_M2", "COUPL_MAG_C4_M1", 
#                       "COUPL_MAG_F3_M2", "COUPL_MAG_F4_M1")]

##N3 fast frontal
#N3_fast.dt <- N3_fast.dt %>%
#  mutate(
#    COUPL_OVERLAP_frontal = rowMeans(across(matches("^COUPL_OVERLAP_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_frontal     = rowMeans(across(matches("^COUPL_MAG_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_frontal   = rowMeans(across(matches("^COUPL_ANGLE_F[34]_M[12]$")), na.rm = TRUE)
#  )
##N3 fast central
#N3_fast.dt <- N3_fast.dt %>%
#  mutate(
#    COUPL_OVERLAP_central = rowMeans(across(matches("^COUPL_OVERLAP_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_central     = rowMeans(across(matches("^COUPL_MAG_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_central   = rowMeans(across(matches("^COUPL_ANGLE_C[34]_M[12]$")), na.rm = TRUE)
#  )


## slow average
#Slow.dt[, couple_angle_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_ANGLE_C3_M2", "COUPL_ANGLE_C4_M1", 
#                    "COUPL_ANGLE_F3_M2", "COUPL_ANGLE_F4_M1")]

#Slow.dt[, couple_overlap_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_OVERLAP_C3_M2", "COUPL_OVERLAP_C4_M1", 
#                    "COUPL_OVERLAP_F3_M2", "COUPL_OVERLAP_F4_M1")]

#Slow.dt[, couple_mag_mean := rowMeans(.SD, na.rm = TRUE),
#        .SDcols = c("COUPL_MAG_C3_M2", "COUPL_MAG_C4_M1", 
#                    "COUPL_MAG_F3_M2", "COUPL_MAG_F4_M1")]
## slow frontal
#Slow.dt <- Slow.dt %>%
#  mutate(
#    COUPL_OVERLAP_frontal = rowMeans(across(matches("^COUPL_OVERLAP_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_frontal     = rowMeans(across(matches("^COUPL_MAG_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_frontal   = rowMeans(across(matches("^COUPL_ANGLE_F[34]_M[12]$")), na.rm = TRUE)
#  )

## slow central
#Slow.dt <- Slow.dt %>%
#  mutate(
#    COUPL_OVERLAP_central = rowMeans(across(matches("^COUPL_OVERLAP_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_central     = rowMeans(across(matches("^COUPL_MAG_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_central   = rowMeans(across(matches("^COUPL_ANGLE_C[34]_M[12]$")), na.rm = TRUE)
#  )

## Averaging N2 slow 
#N2_slow.dt[, couple_angle_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_ANGLE_C3_M2", "COUPL_ANGLE_C4_M1", 
#                       "COUPL_ANGLE_F3_M2", "COUPL_ANGLE_F4_M1")]

#N2_slow.dt[, couple_overlap_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_OVERLAP_C3_M2", "COUPL_OVERLAP_C4_M1", 
#                       "COUPL_OVERLAP_F3_M2", "COUPL_OVERLAP_F4_M1")]

#N2_slow.dt[, couple_mag_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_MAG_C3_M2", "COUPL_MAG_C4_M1", 
#                       "COUPL_MAG_F3_M2", "COUPL_MAG_F4_M1")]

## N2 slow frontal
#N2_slow.dt <- N2_slow.dt %>%
#  mutate(
#    COUPL_OVERLAP_frontal = rowMeans(across(matches("^COUPL_OVERLAP_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_frontal     = rowMeans(across(matches("^COUPL_MAG_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_frontal   = rowMeans(across(matches("^COUPL_ANGLE_F[34]_M[12]$")), na.rm = TRUE)
#  )

## N2 slow central
#N2_slow.dt <- N2_slow.dt %>%
#  mutate(
#    COUPL_OVERLAP_central = rowMeans(across(matches("^COUPL_OVERLAP_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_central     = rowMeans(across(matches("^COUPL_MAG_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_central   = rowMeans(across(matches("^COUPL_ANGLE_C[34]_M[12]$")), na.rm = TRUE)
#  )

## averaging slow N3
#N3_slow.dt[, couple_angle_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_ANGLE_C3_M2", "COUPL_ANGLE_C4_M1", 
#                       "COUPL_ANGLE_F3_M2", "COUPL_ANGLE_F4_M1")]

#N3_slow.dt[, couple_overlap_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_OVERLAP_C3_M2", "COUPL_OVERLAP_C4_M1", 
#                       "COUPL_OVERLAP_F3_M2", "COUPL_OVERLAP_F4_M1")]

#N3_slow.dt[, couple_mag_mean := rowMeans(.SD, na.rm = TRUE),
#           .SDcols = c("COUPL_MAG_C3_M2", "COUPL_MAG_C4_M1", 
#                       "COUPL_MAG_F3_M2", "COUPL_MAG_F4_M1")]

##N3 slow frontal
#N3_slow.dt <- N3_slow.dt %>%
#  mutate(
#    COUPL_OVERLAP_frontal = rowMeans(across(matches("^COUPL_OVERLAP_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_frontal     = rowMeans(across(matches("^COUPL_MAG_F[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_frontal   = rowMeans(across(matches("^COUPL_ANGLE_F[34]_M[12]$")), na.rm = TRUE)
#  )
##N3 slow central
#N3_slow.dt <- N3_slow.dt %>%
#  mutate(
#    COUPL_OVERLAP_central = rowMeans(across(matches("^COUPL_OVERLAP_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_MAG_central     = rowMeans(across(matches("^COUPL_MAG_C[34]_M[12]$")), na.rm = TRUE),
#    COUPL_ANGLE_central   = rowMeans(across(matches("^COUPL_ANGLE_C[34]_M[12]$")), na.rm = TRUE)
#  )

## write to csv
#write.csv(alldt, "filtered_dt.csv", row.names = FALSE)
#write.csv(Fast.dt, "fast.dt.csv", row.names = FALSE)
#write.csv(Slow.dt, "slow.dt.csv", row.names = FALSE)
#write.csv(N2_slow.dt, "N2_slow.dt.csv", row.names = FALSE)
#write.csv(N3_slow.dt, "N3_slow.dt.csv", row.names = FALSE)
#write.csv(N2_fast.dt, "N2_fast.dt.csv", row.names = FALSE)
#write.csv(N3_slow.dt, "N3_fast.dt.csv", row.names = FALSE)

