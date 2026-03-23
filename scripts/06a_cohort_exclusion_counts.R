# -------------------------------------
# Script: 07_combine_exclusions_exposure_outcome.R
# Author: Nick Williams
# Purpose: Combine exclusion/inclusion criteria, exposure, outcome and censoring files.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

treatments <- load_data("followup_treatments.fst", file.path(drv_root, "outcome"))


# base cohort
dts_cohorts <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  filter(BENE_ID %in% treatments$BENE_ID)
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

# exposures
# exposures <- load_data("exposures_with_subsets.fst", file.path(drv_root, "treatments"))
# censoring
# cens <- load_data("pain_washout_continuous_enrollment_opioid_requirements_censoring.fst", file.path(drv_root, "outcome"))
# # outcomes
# outcomes <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_outcomes.fst", file.path(drv_root, "outcome"))

cohort <- list(
  dts_cohorts, 
  opioid_naive,
  debse_exclusions, 
  iph_exclusions, 
  oth_exclusions,
  oud_exclusions
) |> 
  reduce(join, how = "left") |>
  mutate(across(everything(), ~ replace_na(., 0)))


print(paste("Has diagnosis:", nrow(load_data("m4806_washout_dts.fst", file.path(drv_root, "exclusion")))))
print(paste("Has treatment:", nrow(load_data("m4806_cohort_treatment_dts.fst", file.path(drv_root, "exclusion")))))
print(paste("Washout continuously enrolled:", nrow(cohort)))


# exclusion oud
print(paste("Not opioid naive:", sum(cohort$exclusion_opioid_naive)))


cohort <- cohort |>
  filter(exclusion_opioid_naive == 0)

# exclusion oud
print(paste("Has OUD:", sum(cohort$exclusion_oud)))

cohort <- cohort |>
  filter(exclusion_oud == 0)


print(paste("Remaining after OUD and other key exclusions:", nrow(cohort)))

# exclusion MD
print(paste("Maryland:", sum(cohort$exclusion_maryland)))

cohort <- cohort |>
  filter(exclusion_maryland == 0)

# exclusion age
print(paste("Age:", nrow(filter(cohort, exclusion_age == 1 | exclusion_double_bdays == 1))))

cohort <- cohort |>
  filter(exclusion_double_bdays == 0,
         exclusion_age == 0)

# exclusion pregnancy
print(paste("Pregnant:", sum(cohort$exclusion_pregnancy)))

cohort <- cohort |>
  filter(exclusion_pregnancy == 0)

# exclusion unknown sex
print(paste("Missing sex:", sum(cohort$exclusion_missing_sex)))

cohort <- cohort |>
  filter(exclusion_missing_sex == 0)

# exclusion dual eligible
print(paste("Dual eligible:", sum(cohort$exclusion_dual_eligible)))

cohort <- cohort |>
  filter(exclusion_dual_eligible == 0)


# exclusion cancer
print(paste("Cancer:", nrow(filter(cohort, exclusion_cancer == 1 | exclusion_cancer_oth == 1 | exclusion_cancer_iph == 1))))

cohort <- cohort |>
  filter(exclusion_cancer == 0,
         exclusion_cancer_oth == 0,
         exclusion_cancer_iph == 0)

# exclusion institution or palliative
print(paste("Institutionalized:", nrow(filter(cohort, exclusion_pall_iph == 1 | exclusion_pall_oth == 1 | exclusion_institution == 1))))

cohort <- cohort |>
  filter(exclusion_pall_iph == 0,
         exclusion_pall_oth == 0,
         exclusion_institution == 0)

# exclusion inpatient
print(paste("Inpatient:", nrow(filter(cohort, exclusion_monthprior_hospitalization == 1 | exclusion_monthprior_otherinpatient == 1))))

cohort <- cohort |>
  filter(exclusion_monthprior_hospitalization == 0,
         exclusion_monthprior_otherinpatient == 0)

cohort2 <- filter(cohort, if_all(starts_with("exclusion"), \(x) x == 0))
print(paste("Final remaining:", nrow(cohort)))

