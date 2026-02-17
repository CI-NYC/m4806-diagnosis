# -------------------------------------
# Script: 06_tafoth_exclusions.R
# Author: Anton Hung
# Updated:
# Purpose: 
# Notes: Modified from https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/03_initial_cohort_exclusions/clean_tafoth.R
#        (Jan 2026) Addition: exclusion flag for whether or not there is an inpatient
#                   encounter during day 0 and the month prior.
#                   Codes for identifying inpatient encounters in the other services file:
# ┌────────┬────────────┬─────────────────────────────────┐
# │ POS_CD ┆ len        ┆ description                     │
# │ ---    ┆ ---        ┆ ---                             │
# │ str    ┆ u32        ┆ str                             │
# ╞════════╪════════════╪═════════════════════════════════╡
# │ 13     ┆ 11883662   ┆ Assisted Living Facility        │
# │ 32     ┆ 2850792    ┆ Nursing Facility                │
# │ 24     ┆ 2848220    ┆ Ambulatory Surgical Center      │
# │ 55     ┆ 2703649    ┆ Substance Abuse Facility (Resi… │
# │ 31     ┆ 2021273    ┆ Skilled Nursing Facility        │
# │ 09     ┆ 1835769    ┆ Prison/Correctional Facility    │
# │ 51     ┆ 1033784    ┆ Inpatient Psychiatric Facility  │
# -────────-────────────-─────────────────────────────────-
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(collapse)
library(fst)
library(yaml)
library(data.table)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

# Load icd codes
codes <- read_yaml("~/medicaid/M4806-diagnosis-cohort/data/public/icd_codes.yml")
codes_inpatient <- c(13, 21, 32, 24, 55, 31, 09, 41)

# Read in OTH dataset
oth <- open_oth()

oth <- 
  select(oth, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |> 
  inner_join(
    select(cohort, BENE_ID, washout_start_dt, washout_end_dt), 
    by = "BENE_ID"
  ) |> 
  collect() |> 
  fsubset(!(is.na(DGNS_CD_1) & is.na(DGNS_CD_2))) |> 
  fmutate(SRVC_BGN_DT = fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)) |>
  fsubset(SRVC_BGN_DT %within% interval(washout_start_dt, washout_end_dt))

# Identify whether exclusion ICD code of interest occurs in washout ICDs
oth_exclusions_washout <-
  mutate(
    oth, 
    exclusion_pall_oth = +(if_any(starts_with("DGNS_CD"), ~. %in% codes$palliative_care)),
    exclusion_cancer_oth = +(if_any(contains("DGNS_CD"), ~. %in% codes$cancer))
  ) |> 
  fselect(BENE_ID, exclusion_pall_oth, exclusion_cancer_oth)

# checking for inpatient codes, but using a different window: within day -30 and day 0
oth <- open_oth()

oth_monthprior <- 
  select(oth, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, POS_CD) |> 
  inner_join(
    select(cohort, BENE_ID, first_treatment_dt), 
    by = "BENE_ID"
  ) |> 
  collect() |> 
  fsubset(!(is.na(POS_CD))) |> 
  fmutate(SRVC_BGN_DT = fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)) |>
  fsubset(SRVC_BGN_DT %within% interval(first_treatment_dt - days(30), first_treatment_dt))

oth_exclusions_monthprior <-
  mutate(
    oth_monthprior, 
    exclusion_monthprior_otherinpatient = +(POS_CD %in% codes_inpatient)
  ) |> 
  fselect(BENE_ID, exclusion_monthprior_otherinpatient)

# keep only one row per beneficiary
oth_exclusions <- oth_exclusions_washout |>
  full_join(oth_exclusions_monthprior) |>
  mutate(across(exclusion_pall_oth:exclusion_monthprior_otherinpatient, ~ replace_na(.x, 0))) |>
  group_by(BENE_ID) |>
  summarize(across(exclusion_pall_oth:exclusion_monthprior_otherinpatient, ~ fifelse(sum(.x) >= 1, 1, 0)))  |>
  ungroup()

# export
write_data(oth_exclusions, "tafoth_exclusions.fst", file.path(drv_root, "exclusion"))
