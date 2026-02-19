# -------------------------------------
# Script: 00_getting_enrollment_dates.R
# Author: Anton Hung
# Purpose: 
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(dplyr)
library(purrr)


source("~/medicaid/low-back-therapies/R/helpers.R")

# Load temporary files from 01_01_filter_continuous_enrollment.R
files <- file.path(drv_root, "outcome/tmp_post_exposure") |>
  list.files(full.names = TRUE)

washout <- load_data("pain_washout_continuous_enrollment_with_exposures.fst", file.path(drv_root, "treatment")) |>
  select(BENE_ID, exposure_period_end_dt) |>
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
  dt <- dt[, .(exposure_period_end_dt = first(exposure_period_end_dt),
               min_date = min(ENRLMT_START_DT), 
               max_date = max(ENRLMT_END_DT)), by = BENE_ID]
  
  # defined period_1_end_dt, period_2_end_dt, based on exposure_end_dt
  dt[, `:=`(
    period_1_end_dt = exposure_period_end_dt + days(91),
    period_2_end_dt = exposure_period_end_dt + days(182),
    period_3_end_dt = exposure_period_end_dt + days(273),
    period_4_end_dt = exposure_period_end_dt + days(364),
    period_5_end_dt = exposure_period_end_dt + days(455)
  )]
  
  # identify whether each period_end_dt falls within min-max range
  dt[, `:=`(
    enrolled_period_1 = as.integer(period_1_end_dt >= min_date & period_1_end_dt <= max_date),
    enrolled_period_2 = as.integer(period_2_end_dt >= min_date & period_2_end_dt <= max_date),
    enrolled_period_3 = as.integer(period_3_end_dt >= min_date & period_3_end_dt <= max_date),
    enrolled_period_4 = as.integer(period_4_end_dt >= min_date & period_4_end_dt <= max_date),
    enrolled_period_5 = as.integer(period_5_end_dt >= min_date & period_5_end_dt <= max_date)
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
combined_all_df <- bind_rows(results_list)


write_data(
  combined_all_df,
  "enrollment_by_period.fst", file.path(drv_root, "outcome")
)
