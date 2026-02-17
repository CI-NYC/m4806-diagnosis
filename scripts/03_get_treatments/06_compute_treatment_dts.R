# -------------------------------------
# Script: compute_treatment_dts_30_days
# Author: Anton Hung
# Updated:
# Purpose: Looks through all treatments within 3 months of low back pain diagnosis and 
#           identifies the cutoff date for the initial treatment period.
#          The initial treatment period ends when there is more than a 30 day gap
#           until the next treatment, or at the end of 3 months
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

cohort <- load_data("m4806_washout_dts.fst", file.path(drv_root, "exclusion")) |>
  as.data.table() |>
  mutate(treatment_start_dt_possible_latest = msk_diagnosis_dt + days(30))

opioid_dts <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, treatment_start_dt, treatment_end_dt, treatment_name) |> distinct()
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment"))
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment"))

treatments <- rbind(opioid_dts, nop_rx_dts, nonpharma_dts) |> select(-treatment_name) |> as.data.table()

# Keep those with at least 1 treatment within the first 3 months
cohort <- cohort |>
  right_join(treatments) |>
  group_by(BENE_ID) |>
  summarise(first_treatment_dt = min(treatment_start_dt),
            has_treatment = as.numeric(any(treatment_start_dt <= treatment_start_dt_possible_latest))) |>
  filter(has_treatment == 1)


# Keep treatments within 3 months of the first treatment 
# (so, the max time between diagnosis and 1st treatment is 3 months,
# and the max time between diagnosis and end of exposure period is 6 months).
# The exposure period is 3 months long
cohort <- cohort |>
  right_join(treatments) |>
  select(BENE_ID, first_treatment_dt, treatment_start_dt, treatment_end_dt) |>
  filter(treatment_start_dt <= (first_treatment_dt + days(365))) |>
  mutate(treatment_end_dt = pmin(treatment_end_dt, first_treatment_dt + days(365))) |>
  arrange(BENE_ID, treatment_start_dt, treatment_end_dt) |>
  as.data.table()

# cohort <- cohort |>
#   slice(5000000:5200000)

# Nesting dates grouped by BENE_ID --------------------------------------------
cohort <- cohort[, list(data = list(data.table(.SD))), by = BENE_ID]

get_duration <- function(data, gap = 30) {
  # returns the last possible start date for a treatment where the previous treatment is no more than 7 days before.
  
  setDT(data)[
    , treatment_end_dt := as.Date(
      cummax(as.numeric(treatment_end_dt)),
      origin = "1970-01-01"
    )
  ]
  
  
  observation_start_dt <- data$treatment_start_dt
  observation_end_dt   <- data$treatment_end_dt
  
  # next_observation_start <- c(observation_start_dt[-1], NA)
  prev_observation_end <- as.Date(c(NA, observation_end_dt[1:length(observation_end_dt)-1]))
  
  gaps <- as.numeric(observation_start_dt - prev_observation_end - 1)[-1]
  
  # Find the first position where gaps is not NA and ≤ gap
  # Logical vector of valid entries
  valid <- c(T, !is.na(gaps) & gaps <= gap)
  
  # cumall(valid) is TRUE up to the first FALSE/NA, then FALSE thereafter
  lead_ok  <- cumall(valid)
  
  # Extract the last TRUE position, or NA_integer_ if none
  last_idx <- if (any(lead_ok)) max(which(lead_ok)) else NA_integer_
  
  last_elig_treatment_date <- if (!is.na(last_idx)) {
    observation_end_dt[last_idx]
  } else {
    data$treatment_end_dt[1]
  }
  
  result <- data.table(min(last_elig_treatment_date, data$treatment_start_dt_possible_latest[1]))
  
  names(result) <- c("last_treatment_dt")
  return(result)
}

# Looping using a gap of 30 days --------------------------------------------

plan(multisession, workers = 5)
# Apply function
out <- foreach(data = cohort$data,
               id = cohort$BENE_ID,
               .combine = "rbind",
               .options.future = list(chunk.size = 1e3)) %dofuture% {
                 out <- get_duration(data, gap = 30)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }


plan(sequential)
write_data(out, "exposure_end_dt_30_days.fst", file.path(drv_root, "treatment"))


# # Looping using a gap of 7 days --------------------------------------------
# plan(multisession, workers = 10)
# 
# # Apply function
# out_seven <- foreach(id = unique(cohort$BENE_ID), 
#                .combine = "rbind",
#                .options.future = list(chunk.size = 1e3)) %dofuture% {
#                  fsubset(cohort, BENE_ID %==% id) |> 
#                    get_duration(gap = 7)
#                }
# 
# 
# plan(sequential)
# write_data(out_seven, "exposure_end_dt_7_days.fst", file.path(drv_root, "treatment"))