# -------------------------------------
# Script: clean-analytic-file.R
# Author: Nick Williams
# Purpose: Prep data for running the htlmtp estimators
# Notes:
# -------------------------------------

library(dplyr)
library(glue)
library(purrr)

source("R/helpers.R")

raw <- read_raw("stenosis")

lowrisk <- c("Opioid_le7days_le50mme", "Anti-inflammatory", "Muscle relaxant", "Duloxetine", "Physical therapy", "Intervention", "Blocks", "Chiropractic")
highrisk <- c("Opioid_g7days_le50mme", "Opioid_g50mme", "Gabapentin", "Benzodiazepine")

final <- mutate(raw, oud_period_6_12 = as.numeric(rowSums(across(oud_period_6:oud_period_12)) > 0))

final <- mutate(final, 
                dem_race = factor(dem_race), 
                dem_ssi_benefits = factor(dem_ssi_benefits), 
                dem_sex = factor(dem_sex), 
                dem_household_size = factor(dem_household_size))

fst::write_fst(final, "/mnt/general-data/disability/M4806-diagnosis-cohort/history-mtp-application/analytic.fst")
