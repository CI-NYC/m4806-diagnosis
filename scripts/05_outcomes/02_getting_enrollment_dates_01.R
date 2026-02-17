# -------------------------------------
# Script: 00_getting_enrollment_dates.R (part 1)
# Author: Anton Hung
# Purpose: collecting enrollment data for beneficiaries in our cohort
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(doFuture)
library(dplyr)

source("~/medicaid/M4806-diagnosis-cohort//R/helpers.R")

# Load washout dates
washout <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |> 
  mutate(study_end_dt = first_treatment_dt + days(365)) |>
  as.data.table()

## POST - EXPOSURE

# Load all dates
dates <- open_dedts()

dates <- 
  filter(dates, !is.na(BENE_ID)) |> 
  select(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT) |>
  inner_join(washout, by = "BENE_ID") |> 
  collect() |>
  distinct()

setDT(dates, key = "BENE_ID")

dates <- dates[order(rleid(BENE_ID), ENRLMT_START_DT)]

dates <- dates[!is.na(ENRLMT_START_DT) & !is.na(ENRLMT_END_DT)]
dates <- unique(dates[
  ENRLMT_START_DT <= study_end_dt & 
    ENRLMT_END_DT >= first_treatment_dt
][
  , `:=`(
    # “floor” the start date at first_treatment_dt
    ENRLMT_START_DT = fifelse(ENRLMT_START_DT < first_treatment_dt, first_treatment_dt, ENRLMT_START_DT),
    
    # “ceiling” the end date at study_end_dt
    ENRLMT_END_DT = fifelse(ENRLMT_END_DT > study_end_dt, study_end_dt, ENRLMT_END_DT))
][
  , .(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT)
])

idx <- split(seq_len(nrow(dates)), list(dates$BENE_ID))
tmp <- lapply(idx, \(x) dates[x])

rm(idx, washout, dates)
gc()

# Define the function to split a list into chunks
split_list_into_chunks <- function(lst, chunk_size) {
  split(seq_along(lst), ceiling(seq_along(lst) / chunk_size))
}

chunks <- split_list_into_chunks(tmp, 1e5)

# Save each chunk to a separate RDS file
for (i in seq_along(chunks)) {
  file_name <- paste0(drv_root,
    "/outcome/tmp_post_exposure/enrollment_period_chunk_", 
    sprintf("%02d", i), ".rds"
  )
  saveRDS(tmp[chunks[[i]]], file = file_name)
}
