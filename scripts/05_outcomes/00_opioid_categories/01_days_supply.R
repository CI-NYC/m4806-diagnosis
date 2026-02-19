# -------------------------------------
# Script: 04_days_supply.R
# Author: Anton Hung
# Purpose: Calculate the nonoverlapping total days supply of opioids
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

days_supply <- function(data) {
  dur <- 0
  rx_int <- with(data, interval(rx_start, rx_end))
  current_int <- rx_int[1]
  for (i in 1:nrow(data)) {
    check <- intersect(current_int, rx_int[i + 1])
    if (is.na(check)) {
      # if they don't intersect, add the duration of the first interval
      dur <- dur + as.duration(current_int)
      current_int <- rx_int[i + 1]
    } else {
      # if they do intersect, then update current interval as the union
      current_int <- union(current_int, rx_int[i + 1])
    }
  }
  time_length(dur, "days")
}


days_supply_by_month <- function(cohort, month_idx){

  day_start <- (month_idx - 1) * 30
  day_end   <- (month_idx * 30) - 1

  opioids <- load_data("exposure_period_opioids.fst", file.path(drv_root, "treatment")) |>
    left_join(cohort) |>
    fmutate(period_start_dt = first_treatment_dt + days(day_start),
            period_end_dt = first_treatment_dt + days(day_end)) |>
    fsubset(treatment_end_dt >= period_start_dt & treatment_start_dt <= period_end_dt) |>
    roworder(BENE_ID, treatment_start_dt) |> 
    fmutate(treatment_start_dt = pmax(treatment_start_dt, period_start_dt),
            treatment_end_dt = pmin(treatment_end_dt + 1, period_end_dt)) |>
    fselect(BENE_ID, rx_start=treatment_start_dt, rx_end=treatment_end_dt)
  
  days <- foreach(id = unique(opioids$BENE_ID),
                .combine = "rbind",
                .options.future = list(chunk.size = 1e4)) %dofuture% {
    tibble(
      BENE_ID = id,
      days    = fsubset(opioids, BENE_ID %==% id) |> days_supply()
    )
  }
}


plan(multisession, workers = 10)

days_df <- list()

# loop through each month and calculate days supply for that month, then merge all months together at the end
for (i in 1:12){
  days_df[[i]] <- days_supply_by_month(cohort, i) |>
    mutate(days = pmax(days, 1)) |> # some opioids are prescribed on the last day of a period, and are calculated as a 0 before this step
    set_names(c("BENE_ID", paste0("mo",i,"_days_supply")))
}

plan(sequential)

out <- c(list(cohort), days_df)

out <- out |>
  reduce(join, how = "left") |>
  mutate(across(starts_with("mo"), ~replace_na(.x, 0))) |>
  select(BENE_ID, starts_with("mo"))

write_data(out, "days_supply.fst", file.path(drv_root, "outcome"))