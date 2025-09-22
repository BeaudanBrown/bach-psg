# load packages
library(tidyr)
library(data.table)
library(dplyr)

# read in csv as data.table
dc <- fread("data_cleaner.v2.csv")
spindle_dt <- fread("reanalyzed_data.csv")

# merging 
setnames(spindle_dt, "bach_id", "ID")
all.dt <- merge(spindle_dt, dc, by = "ID", all.x = TRUE)


# Regression --------------------------------------------------------------

# logical memory & overlap with fast spindles
overlap_cfast_logical <- lm(logicalmem_delay_total ~ overlap_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_cfast_logical)

overlap_Ffast_logical <- lm(logicalmem_delay_total ~ overlap_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_Ffast_logical)

# logical memory & overlap with slow spindles
overlap_cslow_logical <- lm(logicalmem_delay_total ~ overlap_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_cslow_logical) # overlap sig after adjusting for covar p = 0.012

overlap_Fslow_logical <- lm(logicalmem_delay_total ~ overlap_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_Fslow_logical)

# logical memory & mag with fast spindles
mag_cfast_logical <- lm(logicalmem_delay_total ~ mag_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_cfast_logical)

mag_Ffast_logical <- lm(logicalmem_delay_total ~ mag_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_Ffast_logical)

# logical memory & mag with slow spindles
mag_cslow_logical <- lm(logicalmem_delay_total ~ mag_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_cslow_logical)

mag_Fslow_logical <- lm(logicalmem_delay_total ~ mag_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_Fslow_logical)

# logical memory & angle with fast spindles
angle_cfast_logical <- lm(logicalmem_delay_total ~ angle_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_cfast_logical)

angle_Ffast_logical <- lm(logicalmem_delay_total ~ angle_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_Ffast_logical) # 0.0807 (before adjusting)

# logical memory & angle with slow spindles
angle_cslow_logical <- lm(logicalmem_delay_total ~ angle_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_cslow_logical)

angle_Fslow_logical <- lm(logicalmem_delay_total ~ angle_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_Fslow_logical)

# logical memory and biomarkers
ab_logical <- lm(logicalmem_delay_total ~ ab4240ratio_plasma + age + sex + education_centered, data = all.dt)
summary(ab_logical)

tau217_logical <- lm(logicalmem_delay_total ~ ptau217_mean_conc_plasma + age + sex + education_centered, data = all.dt)
summary(tau217_logical)

gfap_logical <- lm(logicalmem_delay_total ~ gfap_mean_conc_plasma + age + sex + education_centered + apoe_e4_status + psg_tst, data = all.dt)
summary(gfap_logical) # sig if controlling for covariates 

# moderation
overlap_gfap <- lm(logicalmem_delay_total ~ overlap_C_slow * gfap_mean_conc_plasma + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_gfap) # no sig main effects or interaction 

overlap_AB <-  lm(logicalmem_delay_total ~ overlap_C_slow * ab4240ratio_plasma + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_AB)

overlap_tau <-  lm(logicalmem_delay_total ~ overlap_C_slow * ptau217_mean_conc_plasma + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_tau)

#___________
# visual memory

# visual memory & overlap with fast spindles
overlap_cfast_visual <- lm(visualrepro2_total ~ overlap_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_cfast_visual)

overlap_Ffast_visual <- lm(visualrepro2_total ~ overlap_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_Ffast_visual)

# visual memory & overlap with slow spindles
overlap_cslow_visual <- lm(visualrepro2_total ~ overlap_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_cslow_visual) 

overlap_Fslow_visual <- lm(visualrepro2_total ~ overlap_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_Fslow_visual)

# visual memory & mag with fast spindles
mag_cfast_visual <- lm(visualrepro2_total ~ mag_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_cfast_visual)

mag_Ffast_visual <- lm(visualrepro2_total ~ mag_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_Ffast_visual)

# visual memory & mag with slow spindles
mag_cslow_visual <- lm(visualrepro2_total ~ mag_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_cslow_visual)

mag_Fslow_visual <- lm(visualrepro2_total ~ mag_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_Fslow_visual)

# visual memory & angle with fast spindles
angle_cfast_visual <- lm(visualrepro2_total ~ angle_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_cfast_visual)

angle_Ffast_visual <- lm(visualrepro2_total ~ angle_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_Ffast_visual) 

# visual memory & angle with slow spindles
angle_cslow_visual <- lm(visualrepro2_total ~ angle_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_cslow_visual)

angle_Fslow_visual <- lm(visualrepro2_total ~ angle_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_Fslow_visual)
