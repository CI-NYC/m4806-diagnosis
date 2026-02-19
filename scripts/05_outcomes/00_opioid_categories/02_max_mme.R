# -------------------------------------
# Script: 02_max_mme.R
# Author: Anton Hung
# Purpose: Calculate max daily dose (MME) for opioids prescribed during the exposure period
# Notes: Modified to calculate separately for 12 1-month periods
# -------------------------------------

library(tidyverse)
library(readxl)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

# load cohort and opioid data
cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))


calculate_max_daily_dose <- function(data) {
  to_modify <- copy(data)
  
  to_modify[, .(date = seq(treatment_start_dt, treatment_end_dt, by = "1 day"), NDC, opioid, mme_strength_per_day), 
            by = .(seq_len(nrow(data)))
  ][, .(total_mme_strength = sum(mme_strength_per_day, na.rm = TRUE)), 
    by = .(date)
  ][, .(exposure_max_daily_dose_mme = max(total_mme_strength))]
}

mme_by_month <- function(cohort, month_idx){

  day_start <- (month_idx - 1) * 30
  day_end   <- (month_idx * 30) - 1

  opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
    left_join(cohort) |>
    fmutate(period_start_dt = first_treatment_dt + days(day_start),
            period_end_dt = first_treatment_dt + days(day_end)) |>
    fsubset(treatment_end_dt >= period_start_dt & treatment_start_dt <= period_end_dt) |>
    fmutate(treatment_start_dt = pmax(treatment_start_dt, period_start_dt),
            treatment_end_dt = pmin(treatment_end_dt, period_end_dt))

  setDT(opioids)
  setkey(opioids, BENE_ID)

  opioids <- opioids[, .(BENE_ID, first_treatment_dt, 
                        treatment_start_dt, treatment_end_dt, NDC, opioid, mme_strength_per_day)]

  # Calculate max daily dose -----------------------------------------------------

  opioids <- opioids[, list(data = list(data.table(.SD))), by = BENE_ID]

  out <- foreach(data = opioids$data, 
                id = opioids$BENE_ID, 
                .combine = "rbind",
                .options.future = list(chunk.size = 1e4)) %dofuture% {
                  out <- calculate_max_daily_dose(data)
                  out$BENE_ID <- id
                  setcolorder(out, "BENE_ID")
                  out
                }
}


plan(multisession, workers = 10)

mme_df <- list()

for (i in 1:12){
  mme_df[[i]] <- mme_by_month(cohort, i) |>
    set_names(c("BENE_ID", paste0("mo",i,"_max_daily_dose_mme")))
}

plan(sequential)

out <- c(list(cohort), mme_df)

out <- out |>
  reduce(join, how = "left") |>
  mutate(across(starts_with("mo"), ~replace_na(.x, 0))) |>
  select(BENE_ID, starts_with("mo"))

# testthat::test_that(
#   "All observations have a max daily MME",
#   testthat::expect_false(any(is.na(cohort$exposure_max_daily_dose_mme)))
# )

write_data(out, "max_daily_dose_mme.fst", file.path(drv_root, "outcome"))
