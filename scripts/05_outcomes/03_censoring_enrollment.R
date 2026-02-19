# -------------------------------------
# Script: censoring_enrollment.R
# Author: Anton Hung
# Purpose: creating censoring indicators for each period by identifying whether 
#           each beneficiary's enrollment dates cover the last day of a given period
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(dplyr)
library(purrr)
library(lmtp)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

# Load temporary files from 01_01_filter_continuous_enrollment.R
files <- file.path(drv_root, "outcome/tmp_post_exposure") |>
  list.files(full.names = TRUE)

washout <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  select(BENE_ID, washout_end_dt) |>
  as.data.table()

# evaluating each chunk one at a time
for (i in seq_along(files)) { 
  tmp <- readRDS(files[i])
  
  # only keep those in the cohort
  tmp <- tmp[names(tmp) %in% washout$BENE_ID]
  
  # combine into 1 large dt
  tmp <- as.data.table(bind_rows(tmp))
  
  # join dt to washout using data table
  dt <- washout[tmp, on = "BENE_ID", nomatch = 0]
  
  # defined period_1_end_dt, period_2_end_dt, etc. based on washout_start_dt
  dt[, c(paste0("period_", 1:num_periods, "_end_dt")) := 
    lapply(1:num_periods, function(p) washout_end_dt + days(p * follow_up_period_length))]
  
  # identify whether each period_end_dt falls within min-max range
  dt <- dt[, lapply(
    # create a vector of names for the output columns
    setNames(1:num_periods, paste0("enrolled_period_", 1:num_periods)), 
    # apply function to each period number, which checks if the corresponding period_end_dt falls within any enrollment period
    function(p) as.integer(any(
      get(paste0("period_", p, "_end_dt")) %between% 
        .(ENRLMT_START_DT, ENRLMT_END_DT)))
  ), by = BENE_ID]
  
  write_data(
    dt,
    paste0("all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_", sprintf("%02d", i), ".fst"),
    file.path(drv_root, "outcome")
  )
}

# combining results into a list
results_list <- list()
for (i in seq_along(files)){
  print(i)
  final_df <- load_data(paste0("outcome/all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_", sprintf("%02d", i), ".fst"), drv_root)
  
  results_list[[i]] <- final_df
}

# combining list into a dataframe
cens_enrollment_df <- bind_rows(results_list) |>
  # flip enrollment indicators to censoring indicators
  mutate(across(starts_with("enrolled_period_"), ~ 1 - .x)) |>
  # now that they are flipped, they should be renamed to censoring columns
  rename_with(~ gsub("enrolled", "cens_enrollment", .x), starts_with("enrolled_period_")) |>
  # ensure that once censored, remain censored
  lmtp::event_locf(paste0("cens_enrollment_period_", 1:num_periods)) |>
  select(-ends_with("dt"))


write_data(
  cens_enrollment_df,
  "enrollment_by_period.fst", file.path(drv_root, "outcome")
)
