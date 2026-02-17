# -------------------------------------
# Script: opioid naive
# Author: Anton Hung
# Purpose: Like 03_get_treatments/02_cohort_mme_join.R, this script identifies all
#           opioids given to patients.
#          Here, patients are flagged for exclusion if an opioid is found in the washout period.
# Notes:
# -------------------------------------

library(data.table)
library(tidyverse)
library(yaml)
library(foreach)
library(fst)
library(arrow)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

ndc <- readRDS("~/medicaid/M4806-diagnosis-cohort/data/public/ndc_to_atc_crosswalk.rds")
codes <- read_yaml("~/medicaid/M4806-diagnosis-cohort/data/public/drug_codes.yml")

# load initial continuous enrollment cohort
cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  as.data.table()

ndc_opioids <- readRDS("~/medicaid/M4806-diagnosis-cohort/data/public/ndc_to_atc_opioids.rds")



### LOOKING FOR OPIOIDS IN WASHOUT

# Read in RXL (pharmacy line)
rxl <- open_rxl()

# Read in OTL (Other services line)
otl <- open_otl()

# Find beneficiaries with an opioid prior to MSK diagnosis in OTL (washout)
otl <-
  select(otl, BENE_ID, CLM_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, NDC) |>
  inner_join(cohort, by = "BENE_ID") |>
  mutate(LINE_SRVC_BGN_DT = ifelse(
    is.na(LINE_SRVC_BGN_DT),
    LINE_SRVC_END_DT,
    LINE_SRVC_BGN_DT),
    LINE_SRVC_END_DT = ifelse(
      is.na(LINE_SRVC_END_DT),
      LINE_SRVC_BGN_DT,
      LINE_SRVC_END_DT),
    LINE_SRVC_BGN_DT = ifelse(LINE_SRVC_BGN_DT < washout_start_dt & LINE_SRVC_END_DT >= washout_start_dt, washout_start_dt, LINE_SRVC_BGN_DT) # if service began before but ends after washout, then consider start as washout
  ) |>
  filter((LINE_SRVC_BGN_DT < msk_diagnosis_dt & (LINE_SRVC_BGN_DT >= washout_start_dt)),
         NDC %in% ndc_opioids$NDC) |>
  select(BENE_ID) |>
  distinct()

otl <- collect(otl) |> as.data.table()

# Find beneficiaries with an opioid prior to MSK diagnosis in RXL (washout)
rxl <-
  select(rxl, BENE_ID, CLM_ID, RX_FILL_DT, DAYS_SUPPLY, NDC) |>
  inner_join(cohort, by = "BENE_ID") |>
  collect() |>
  mutate(RX_END_DT = RX_FILL_DT + DAYS_SUPPLY - 1,
         RX_FILL_DT = ifelse(RX_FILL_DT < washout_start_dt & RX_END_DT >= washout_start_dt, washout_start_dt, RX_FILL_DT) # if service began before but ends after washout, then consider start as washout
  ) |>
  select(-c(DAYS_SUPPLY)) |>
  filter((RX_FILL_DT < msk_diagnosis_dt & (RX_FILL_DT >= washout_start_dt)),
         NDC %in% ndc_opioids$NDC) |>
  select(BENE_ID) |>
  distinct()

rxl <- rxl |> as.data.table()

# remove observations with opioid in washout period
remove <- rbind(otl, rxl) |> unique()

# number of patients with opioids prior to MSK diagnosis
remove |> nrow()

# cohort_with_opioids_none_in_washout <- anti_join(cohort, remove)

cohort <- cohort |>
  mutate(exclusion_opioid_naive = ifelse(BENE_ID %in% remove$BENE_ID, 1, 0)) |>
  select(BENE_ID, exclusion_opioid_naive)

write_data(cohort, "opioid_naive.fst", file.path(drv_root,"exclusion"))
