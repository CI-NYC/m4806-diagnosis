# -------------------------------------
# Script: finalize_treatment_dts
# Author: Anton Hung
# Updated:
# Purpose: Looks through compiled treatments after low back pain diagnosis and 
#           Keeps treatments only if they are within a 30 day (or 7 day) gap
#           of the previous treatment.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)


source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("m4806_washout_dts.fst", file.path(drv_root, "exclusion"))

opioid_dts <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, treatment_start_dt, treatment_end_dt, treatment_name) |> distinct()
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment"))
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment"))
treatment_end_dt <- load_data("exposure_end_dt_30_days.fst", file.path(drv_root, "treatment"))

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> 
  right_join(treatment_end_dt) |>
  filter(treatment_start_dt <= last_treatment_dt) |>
  as.data.table()


# Collect all treatments -------------------------------------------------------

cohort_dts <- cohort |>
  right_join(treatments) |>
  group_by(BENE_ID) |>
  mutate(first_treatment_dt = min(treatment_start_dt)) |>
  as.data.table() |>
  select(BENE_ID, msk_diagnosis_dt, first_treatment_dt, last_treatment_dt) |>
  distinct()

write_data(cohort_dts, "m4806_cohort_treatment_dts.fst", file.path(drv_root, "exclusion"))

# 90 307 people are treated (out of a possible 122 192)
