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
  
  # group by bene_id, define min and max dates
  dt <- dt[, .(washout_end_dt = first(washout_end_dt),
               min_dt = min(ENRLMT_START_DT), 
               max_dt = max(ENRLMT_END_DT)), by = BENE_ID]
  
  # defined period_1_end_dt, period_2_end_dt, based on washout_start_dt
  dt[, `:=`(
    period_1_end_dt = washout_end_dt + days(30),
    period_2_end_dt = washout_end_dt + days(60),
    period_3_end_dt = washout_end_dt + days(90),
    period_4_end_dt = washout_end_dt + days(120),
    period_5_end_dt = washout_end_dt + days(150),
    period_6_end_dt = washout_end_dt + days(180),
    period_7_end_dt = washout_end_dt + days(210),
    period_8_end_dt = washout_end_dt + days(240),
    period_9_end_dt = washout_end_dt + days(270),
    period_10_end_dt = washout_end_dt + days(300),
    period_11_end_dt = washout_end_dt + days(330),
    period_12_end_dt = washout_end_dt + days(360)
  )]
  
  # identify whether each period_end_dt falls within min-max range
  dt[, `:=`(
    enrolled_period_1 = as.integer(period_1_end_dt >= min_dt & period_1_end_dt <= max_dt),
    enrolled_period_2 = as.integer(period_2_end_dt >= min_dt & period_2_end_dt <= max_dt),
    enrolled_period_3 = as.integer(period_3_end_dt >= min_dt & period_3_end_dt <= max_dt),
    enrolled_period_4 = as.integer(period_4_end_dt >= min_dt & period_4_end_dt <= max_dt),
    enrolled_period_5 = as.integer(period_5_end_dt >= min_dt & period_5_end_dt <= max_dt),
    enrolled_period_6 = as.integer(period_6_end_dt >= min_dt & period_6_end_dt <= max_dt),
    enrolled_period_7 = as.integer(period_7_end_dt >= min_dt & period_7_end_dt <= max_dt),
    enrolled_period_8 = as.integer(period_8_end_dt >= min_dt & period_8_end_dt <= max_dt),
    enrolled_period_9 = as.integer(period_9_end_dt >= min_dt & period_9_end_dt <= max_dt),
    enrolled_period_10 = as.integer(period_10_end_dt >= min_dt & period_10_end_dt <= max_dt),
    enrolled_period_11 = as.integer(period_11_end_dt >= min_dt & period_11_end_dt <= max_dt),
    enrolled_period_12 = as.integer(period_12_end_dt >= min_dt & period_12_end_dt <= max_dt)
  )]
  
  
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
  lmtp::event_locf(paste0("cens_enrollment_period_", 1:12)) |>
  select(-ends_with("dt"))


write_data(
  cens_enrollment_df,
  "enrollment_by_period.fst", file.path(drv_root, "outcome")
)
