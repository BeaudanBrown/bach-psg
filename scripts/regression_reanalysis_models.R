# Exploratory regression script retained outside the targets pipeline.
# This file is intentionally separate from the reproducible pipeline entrypoints
# in `R/` and `_targets.R`.

library(tidyr)
library(data.table)
library(dplyr)

dc <- fread("data_cleaner.v2.csv")
spindle_dt <- fread("reanalyzed_data.csv")

setnames(spindle_dt, "bach_id", "ID")
all.dt <- merge(spindle_dt, dc, by = "ID", all.x = TRUE)

overlap_cfast_logical <- lm(logicalmem_delay_total ~ overlap_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_cfast_logical)

overlap_Ffast_logical <- lm(logicalmem_delay_total ~ overlap_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_Ffast_logical)

overlap_cslow_logical <- lm(logicalmem_delay_total ~ overlap_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_cslow_logical)

overlap_Fslow_logical <- lm(logicalmem_delay_total ~ overlap_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_Fslow_logical)

mag_cfast_logical <- lm(logicalmem_delay_total ~ mag_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_cfast_logical)

mag_Ffast_logical <- lm(logicalmem_delay_total ~ mag_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_Ffast_logical)

mag_cslow_logical <- lm(logicalmem_delay_total ~ mag_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_cslow_logical)

mag_Fslow_logical <- lm(logicalmem_delay_total ~ mag_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_Fslow_logical)

angle_cfast_logical <- lm(logicalmem_delay_total ~ angle_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_cfast_logical)

angle_Ffast_logical <- lm(logicalmem_delay_total ~ angle_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_Ffast_logical)

angle_cslow_logical <- lm(logicalmem_delay_total ~ angle_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_cslow_logical)

angle_Fslow_logical <- lm(logicalmem_delay_total ~ angle_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_Fslow_logical)

ab_logical <- lm(logicalmem_delay_total ~ ab4240ratio_plasma + age + sex + education_centered, data = all.dt)
summary(ab_logical)

tau217_logical <- lm(logicalmem_delay_total ~ ptau217_mean_conc_plasma + age + sex + education_centered, data = all.dt)
summary(tau217_logical)

gfap_logical <- lm(logicalmem_delay_total ~ gfap_mean_conc_plasma + age + sex + education_centered + apoe_e4_status + psg_tst, data = all.dt)
summary(gfap_logical)

overlap_gfap <- lm(logicalmem_delay_total ~ overlap_C_slow * gfap_mean_conc_plasma + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_gfap)

overlap_AB <- lm(logicalmem_delay_total ~ overlap_C_slow * ab4240ratio_plasma + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_AB)

overlap_tau <- lm(logicalmem_delay_total ~ overlap_C_slow * ptau217_mean_conc_plasma + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_tau)

overlap_cfast_visual <- lm(visualrepro2_total ~ overlap_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_cfast_visual)

overlap_Ffast_visual <- lm(visualrepro2_total ~ overlap_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_Ffast_visual)

overlap_cslow_visual <- lm(visualrepro2_total ~ overlap_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_cslow_visual)

overlap_Fslow_visual <- lm(visualrepro2_total ~ overlap_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(overlap_Fslow_visual)

mag_cfast_visual <- lm(visualrepro2_total ~ mag_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_cfast_visual)

mag_Ffast_visual <- lm(visualrepro2_total ~ mag_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_Ffast_visual)

mag_cslow_visual <- lm(visualrepro2_total ~ mag_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_cslow_visual)

mag_Fslow_visual <- lm(visualrepro2_total ~ mag_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(mag_Fslow_visual)

angle_cfast_visual <- lm(visualrepro2_total ~ angle_C_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_cfast_visual)

angle_Ffast_visual <- lm(visualrepro2_total ~ angle_F_fast + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_Ffast_visual)

angle_cslow_visual <- lm(visualrepro2_total ~ angle_C_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_cslow_visual)

angle_Fslow_visual <- lm(visualrepro2_total ~ angle_F_slow + age + sex + education_centered + apoe_e4_status + psg_ahi_total_nrem + psg_tst + psg_waso, data = all.dt)
summary(angle_Fslow_visual)
