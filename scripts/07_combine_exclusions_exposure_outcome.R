# -------------------------------------
# Script: 07_combine_exclusions_exposure_outcome.R
# Author: Anton Hung
# Purpose: Combine exclusion/inclusion criteria, exposure, outcome and censoring files.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)
library(data.table)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

# base cohort
cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))
# opioid naive
opioid_naive <- load_data("opioid_naive.fst", file.path(drv_root,"exclusion"))
# debse exclusions
debse_exclusions <- load_data("tafdebse_exclusions.fst", file.path(drv_root, "exclusion"))
# iph exclusions
iph_exclusions <- load_data("tafiph_exclusions.fst", file.path(drv_root, "exclusion"))
# oth exclusions
oth_exclusions <- load_data("tafoth_exclusions.fst", file.path(drv_root, "exclusion"))
# oud exclusions
oud_exclusions <- load_data("oud_exclusion.fst", file.path(drv_root, "exclusion"))
# censoring
cens <- load_data("msk_washout_continuous_enrollment_censoring.fst", file.path(drv_root, "outcome"))
# outcomes
treatments <- load_data("followup_treatments.fst", file.path(drv_root, "outcome"))
outcomes <- load_data("msk_washout_continuous_enrollment_oud_outcomes.fst", file.path(drv_root, "outcome"))
hillary <- load_data("msk_washout_continuous_enrollment_oud_hillary_outcomes.fst", file.path(drv_root, "outcome"))

cohort <- list(
  cohort, 
  opioid_naive,
  debse_exclusions, 
  iph_exclusions, 
  oth_exclusions, 
  oud_exclusions
) |> 
  reduce(left_join) |>
  mutate(across(everything(), ~ replace_na(., 0)))

# Remove observations with exclusions
cohort <- filter(cohort, if_all(starts_with("exclusion"), \(x) x == 0))

# Add in exposure, outcome, and censoring data
cohort <- list(cohort,
               treatments,
               outcomes,
               hillary,
               cens) |>
  reduce(left_join) |>
  mutate(across(everything(), ~ replace_na(., 0)))



cohort <- cohort |>
  select(-starts_with("exclusion"),
         -ends_with("exposure"))
  

write_data(cohort, "inclusion_exclusion_cohort_with_outcomes.fst", file.path(drv_root, "final"))

# 13278