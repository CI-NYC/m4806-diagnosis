# -------------------------------------
# Script: 00_getting_enrollment_dates.R (part 2)
# Author: Anton Hung
# Purpose: Create a dataframe for all people in cohort containing every possible date of enrollment and an indicator 
#   for whether or not they were enrolled on that date.
# Notes: Part 2 and Part 3 compute the same steps, but breaking up the data into 
#         chunks of 1 million people so that they can be run in parallel.
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
#library(doFuture)
library(dplyr)
library(purrr)
# library(future)
# library(furrr)
# library(doParallel)
# options(cores = 30)
# registerDoParallel()
# plan(multicore)
# getDoParWorkers()


source("~/medicaid/M4806-diagnosis-cohort//R/helpers.R")

# Load temporary files from 01_01_filter_continuous_enrollment.R
files <- file.path(drv_root, "outcome/tmp_post_exposure") |>
  list.files(full.names = TRUE)

# get BENE_ID of cohort
washout <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  select(BENE_ID, first_treatment_dt)

washout_ID <- washout$BENE_ID

# gets all possble enrollment dates (using the minimum start and maximum end date) then creates an indicator for whether or not they were enrolled on that date
getdates <- function(df) {
  
  dt <- as.data.table(df)
  
  min_start_date <- min(dt$ENRLMT_START_DT, na.rm = TRUE) # earliest possible start date
  max_end_date <- max(dt$ENRLMT_END_DT, na.rm = TRUE) # latest possible end date
  all_dates <- data.table(date = seq.Date(min_start_date, max_end_date, by = "day")) # all possible enrollment dates for individual z
  
  # # make all possible enrolled dates into a dataframe
  # all_possible_enrollment_dates_df <- data.frame(date = all_possible_enrollment_dates)
  
  # # for each row (enrollment period), make all possible enrollment dates, then combine across rows and get distinct dates
  # enrolled_dates_df <- do.call(rbind, apply(df, 1, function(row) {
  #   dates <- seq.Date(from = as.Date(row["ENRLMT_START_DT"]), to = as.Date(row["ENRLMT_END_DT"]), by = "day")
  #   data.frame(date = dates) 
  # })) |>
  #   distinct() |>
  #   mutate(enrolled = 1)
  
  enrolled_dates <- dt[all_dates, 
                       on = .(ENRLMT_START_DT <= date, ENRLMT_END_DT >= date), 
                       nomatch = 0, 
                       .(date)]
  
  enrolled_dates <- unique(enrolled_dates)[, enrolled := 1]
  
  result <- merge(all_dates, enrolled_dates, by = "date", all.x = TRUE)
  result[is.na(enrolled), enrolled := 0]
  
  return(result)
}

# applying function to each chunk
for (i in 1) {
  tmp <- readRDS(files[i])
  
  # only keep those in the cohort
  #tmp <- tmp[names(tmp) %in% washout_ID]
  
  # combine into 1 large df
  tmp_df <- bind_rows(tmp) 
  
  # get dates for each beneficiary and return as a df
  final_df <- tmp_df |>
    group_by(BENE_ID) |>
    group_map(~ {
      output <- getdates(.x)
      bind_cols(.y, output)
    }) |>
    bind_rows()
  
  write_data(
    final_df,
    paste0("all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_", sprintf("%02d", i), ".fst"),
    file.path(drv_root, "outcome")
  )
}

# # combining results into a list
results_list <- list()
for (i in seq_along(files)){
  print(i)
  final_df <- load_data(paste0("outcome/all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_", sprintf("%02d", i), ".fst"), drv_root)
    # filter(BENE_ID %in% washout_ID) |>
    # left_join(washout) |>
    # filter(date >= first_treatment_dt) |> # only need dates from post-exposure start
    # select(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT)

  results_list[[i]] <- final_df
}
#
# # combining list into a dataframe
combined_all_df <- bind_rows(results_list)


write_data(
  combined_all_df,
  paste0("all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_ALL", ".fst"), file.path(drv_root, "outcome")
)
