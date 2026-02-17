# -------------------------------------
# Script: drug_use_disorder
# Author: Anton Hung 
# Purpose: Identify substance use disorder during the washout period
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(arrow)
library(data.table)
library(yaml)

# claims data
source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("msk_cohort.fst", file.path(drv_root, "final"))

# Read in OTH
oth <- open_oth()

# Read in IPH
iph <- open_iph()

codebook <- read_yaml("/home/amh2389/medicaid/low-back-therapies/data/public/mediator_codes.yml")

find_substance <- function(which_substance, new_column_name) {
  codes <- (names(codebook[[which_substance]]$ICD10))
  pattern <- paste(codes, collapse = "|")
  
  substance_oth <- oth |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
    filter(BENE_ID %in% cohort$BENE_ID) |>
    filter(if_any(starts_with("DGNS_CD"), ~ grepl(pattern, .))) |>
    mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
    collect() |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT)
  
  substance_iph <- iph |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
    filter(BENE_ID %in% cohort$BENE_ID) |>
    filter(if_any(starts_with("DGNS_CD"), ~ grepl(pattern, .))) |>
    mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
    collect() |>
    select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT)
  
  has_substance_abuse <- rbind(substance_oth, substance_iph) |>
    right_join(cohort) |>
    filter(SRVC_BGN_DT %within% interval(washout_start_dt, washout_end_dt)) |>
    group_by(BENE_ID) |>
    mutate(!!new_column_name := 1) |>
    ungroup() |>
    select(BENE_ID, !!new_column_name) |>
    distinct()
}

### ALCOHOL
sud_alcohol_washout_cal <- find_substance("Alcohol", "sud_alcohol_washout_cal")

### OTHER SUBSTANCES
sud_other_washout_cal <- find_substance("Other substances", "sud_other_washout_cal")


### Putting all together
cohort <- cohort |>
  select(BENE_ID) |>
  left_join(sud_alcohol_washout_cal) |>
  left_join(sud_other_washout_cal) |>
  mutate(across(starts_with("sud_"), ~replace(., is.na(.), 0)))

write_data(cohort, "substance_use_disorder_washout_cal.fst", file.path(drv_root, "baseline"))

