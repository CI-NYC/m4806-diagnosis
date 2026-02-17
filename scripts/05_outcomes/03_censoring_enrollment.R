# -------------------------------------
# Script: 00_censoring.R
# Author: Nick Williams
# Purpose: Create censoring indicators
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(dplyr)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

# reading in cohorts
cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  select(BENE_ID, first_treatment_dt)

all_enrollment_dates <- load_data("all_possible_enrollment_dates/combined_all_enrolled_dates_cohort_ALL.fst", file.path(drv_root, "outcome")) |>
  left_join(cohort) |>
  filter(date >= first_treatment_dt) # only need dates from post-exposure start


# creating exposure end date periods (each period is 91 days long)

all_enrollment_dates <- all_enrollment_dates |>
  mutate(period_1_end_dt = first_treatment_dt + days(29),
         period_2_end_dt = period_1_end_dt + days(30),
         period_3_end_dt = period_2_end_dt + days(30),
         period_4_end_dt = period_3_end_dt + days(30),
         period_5_end_dt = period_4_end_dt + days(30),
         period_6_end_dt = period_5_end_dt + days(30),
         period_7_end_dt = period_6_end_dt + days(30),
         period_8_end_dt = period_7_end_dt + days(30),
         period_9_end_dt = period_8_end_dt + days(30),
         period_10_end_dt = period_9_end_dt + days(30),
         period_11_end_dt = period_10_end_dt + days(30),
         period_12_end_dt = period_11_end_dt + days(30)
  )

# creating indicator for each period

all_enrollment_dates <- all_enrollment_dates |>
  mutate(enrolled_during_period_1 = case_when(date >= first_treatment_dt & date <= period_1_end_dt & enrolled == 1 ~ 1,
                                              TRUE ~ 0),
         enrolled_during_period_2 = case_when(date >= period_1_end_dt + days(0) & date <= period_2_end_dt & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_3 = case_when(date >= period_2_end_dt + days(0) & date <= period_3_end_dt & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_4 = case_when(date >= period_3_end_dt + days(0) & date <= period_4_end_dt & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_5 = case_when(date >= period_4_end_dt + days(0) & date <= period_5_end_dt & enrolled == 1 ~ 1, # if they were enrolled on the LAST day of the previous period, their censoring should start in this period
                                              TRUE ~ 0),
         enrolled_during_period_6 = case_when(date >= period_5_end_dt + days(0) & date <= period_6_end_dt & enrolled == 1 ~ 1, TRUE ~ 0),
         enrolled_during_period_7 = case_when(date >= period_6_end_dt + days(0) & date <= period_7_end_dt & enrolled == 1 ~ 1, TRUE ~ 0),
         enrolled_during_period_8 = case_when(date >= period_7_end_dt + days(0) & date <= period_8_end_dt & enrolled == 1 ~ 1, TRUE ~ 0),
         enrolled_during_period_9 = case_when(date >= period_8_end_dt + days(0) & date <= period_9_end_dt & enrolled == 1 ~ 1, TRUE ~ 0),
         enrolled_during_period_10 = case_when(date >= period_9_end_dt + days(0) & date <= period_10_end_dt & enrolled == 1 ~ 1, TRUE ~ 0),
         enrolled_during_period_11 = case_when(date >= period_10_end_dt + days(0) & date <= period_11_end_dt & enrolled == 1 ~ 1, TRUE ~ 0),
         enrolled_during_period_12 = case_when(date >= period_11_end_dt + days(0) & date <= period_12_end_dt & enrolled == 1 ~ 1, TRUE ~ 0),
         study_end_dt = first_treatment_dt + days(365),
         enrolled_during_study_end_dt = case_when(date == study_end_dt & enrolled == 1 ~ 1,
                                                  TRUE ~ 0),
  )

all_enrollment_dates_grouped <- all_enrollment_dates |>
  group_by(BENE_ID) |>
  summarize(enrollment_cens_period_1 = any(enrolled_during_period_1 == 1),
            enrollment_cens_period_2 = any(enrolled_during_period_2 == 1),
            enrollment_cens_period_3 = any(enrolled_during_period_3 == 1),
            enrollment_cens_period_4 = any(enrolled_during_period_4 == 1),
            enrollment_cens_period_5 = any(enrolled_during_period_5 == 1),
            enrollment_cens_period_6 = any(enrolled_during_period_6 == 1),
            enrollment_cens_period_7 = any(enrolled_during_period_7 == 1),
            enrollment_cens_period_8 = any(enrolled_during_period_8 == 1),
            enrollment_cens_period_9 = any(enrolled_during_period_9 == 1),
            enrollment_cens_period_10 = any(enrolled_during_period_10 == 1),
            enrollment_cens_period_11 = any(enrolled_during_period_11 == 1),
            enrollment_cens_period_12 = any(enrolled_during_period_12 == 1),
            enrollment_cens_study_end_dt = any(enrolled_during_study_end_dt == 1)
  ) |>
  mutate(enrollment_cens_period_1 = case_when(enrollment_cens_period_1 == FALSE ~ 1, # disenrolled prior to start of outcome period
                                              enrollment_cens_period_2 == FALSE ~ 1, # not enrolled at all during next period, then they are censored during this period
                                              TRUE ~ 0), 
         enrollment_cens_period_2 =  case_when(enrollment_cens_period_1 == 1 ~ 1,
                                               enrollment_cens_period_3 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_3 =  case_when(enrollment_cens_period_2 == 1 ~ 1,
                                               enrollment_cens_period_4 == FALSE ~ 1,
                                               TRUE ~ 0),
         enrollment_cens_period_4 =  case_when(enrollment_cens_period_3 == 1 ~ 1,
                                               enrollment_cens_period_5 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_5 =  case_when(enrollment_cens_period_4 == 1 ~ 1,
                                               enrollment_cens_period_6 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_6 =  case_when(enrollment_cens_period_5 == 1 ~ 1,
                                               enrollment_cens_period_7 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_7 =  case_when(enrollment_cens_period_6 == 1 ~ 1,
                                               enrollment_cens_period_8 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_8 =  case_when(enrollment_cens_period_7 == 1 ~ 1,
                                               enrollment_cens_period_9 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_9 =  case_when(enrollment_cens_period_8 == 1 ~ 1,
                                               enrollment_cens_period_10 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_10 =  case_when(enrollment_cens_period_9 == 1 ~ 1,
                                                enrollment_cens_period_11 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_11 =  case_when(enrollment_cens_period_10 == 1 ~ 1,
                                                enrollment_cens_period_12 == FALSE ~ 1,
                                               TRUE ~ 0), 
         enrollment_cens_period_12 =  case_when(enrollment_cens_period_11 == 1 ~ 1,
                                               enrollment_cens_study_end_dt == FALSE ~ 1,
                                               TRUE ~ 0)
         
  ) |>
  select(-enrollment_cens_study_end_dt)


write_data(all_enrollment_dates_grouped, "all_possible_enrollment_dates/censoring_enrollment.fst", file.path(drv_root, "outcome"))


