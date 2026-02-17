# -------------------------------------
# Script: 01_01_filter_continuous_enrollment.R
# Author: Nick Williams
# Purpose: Split enrollment periods into chunks per beneficiary
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(doFuture)
library(dplyr)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

# Load washout dates
washout <- load_data("m4806_cohort_treatment_dts.fst", file.path(drv_root, "exclusion")) |> as.data.table()
# 90 326

washout[, let(washout_start_dt = first_treatment_dt - days(182),
              washout_end_dt = first_treatment_dt - days(1))]

# Load all dates
dates <- open_dedts()

dates <- 
  filter(dates, !is.na(BENE_ID)) |> 
  select(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT) |>
  inner_join(washout, by = "BENE_ID") |> 
  collect()

setDT(dates, key = "BENE_ID")

dates <- dates[order(rleid(BENE_ID), ENRLMT_START_DT)]
dates <- dates[!is.na(ENRLMT_START_DT) & !is.na(ENRLMT_END_DT)]
dates <- dates[ENRLMT_START_DT <= washout_end_dt]

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
    "/exclusion/tmp/enrollment_period_chunk_", 
    i, ".rds"
  )
  saveRDS(tmp[chunks[[i]]], file = file_name)
}
