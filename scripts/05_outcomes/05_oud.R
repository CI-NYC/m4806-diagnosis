# -------------------------------------
# Script: 00_oud.R
# Author: Nick Williams
# Purpose: Create composite OUD survival outcome
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)
library(lubridate)
library(data.table)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")


cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

# load component files ----------------------------------------------------

poison <- load_data(paste0("oud_poison_dts.fst"), file.path(drv_root, "exclusion"))
hillary <- load_data(paste0("oud_hillary_dts.fst"), file.path(drv_root, "exclusion"))
opioids <- load_data(paste0("pain_opioids_dts.fst"), file.path(drv_root, "exclusion"))
bup <- load_data(paste0("moud_bup_intervals.fst"), file.path(drv_root, "treatment"))
methadone <- load_data(paste0("moud_methadone_intervals.fst"), file.path(drv_root, "treatment"))
nal <- load_data(paste0("moud_nal_intervals.fst"), file.path(drv_root, "treatment"))

# make outcome variable indicators ----------------------------------------

cohort <- as_tibble(cohort)

# Create outcome periods
cohort <- fmutate(
  cohort, 
  period_1 = interval(
    first_treatment_dt, first_treatment_dt + days(29) # exposure period + period 1
  ), 
  period_2 = interval(
    int_end(period_1) + days(1), int_end(period_1) + days(30)
  ), 
  period_3 = interval(
    int_end(period_2) + days(1), int_end(period_2) + days(30)
  ), 
  period_4 = interval(
    int_end(period_3) + days(1), int_end(period_3) + days(30)
  ), 
  period_5 = interval(
    int_end(period_4) + days(1), int_end(period_4) + days(30)
  ),
  period_6 = interval(int_end(period_5) + days(1), int_end(period_5) + days(30)),
  period_7 = interval(int_end(period_6) + days(1), int_end(period_6) + days(30)),
  period_8 = interval(int_end(period_7) + days(1), int_end(period_7) + days(30)),
  period_9 = interval(int_end(period_8) + days(1), int_end(period_8) + days(30)),
  period_10 = interval(int_end(period_9) + days(1), int_end(period_9) + days(30)),
  period_11 = interval(int_end(period_10) + days(1), int_end(period_10) + days(30)),
  period_12 = interval(int_end(period_11) + days(1), int_end(period_11) + days(30))
)

in_period <- function(data, date_col, period_col, overlap = FALSE, prefix) {
  if (isFALSE(overlap)) {
    return(mutate(data, "{prefix}_{{ period_col }}" := as.numeric({{ date_col }} %within% {{ period_col }})))
  }
  mutate(data, "{prefix}_{{ period_col }}" := as.numeric(int_overlaps({{ date_col }}, {{ period_col }})))
}

add_all_periods <- function(x, y, date_col, overlap, prefix) {
  left_join(x, y) |> 
    in_period({{ date_col }}, period_1, overlap, prefix) |> 
    in_period({{ date_col }}, period_2, overlap, prefix) |> 
    in_period({{ date_col }}, period_3, overlap, prefix) |> 
    in_period({{ date_col }}, period_4, overlap, prefix) |> 
    in_period({{ date_col }}, period_5, overlap, prefix)|> 
    in_period({{ date_col }}, period_6, overlap, prefix)|> 
    in_period({{ date_col }}, period_7, overlap, prefix)|> 
    in_period({{ date_col }}, period_8, overlap, prefix)|> 
    in_period({{ date_col }}, period_9, overlap, prefix)|> 
    in_period({{ date_col }}, period_10, overlap, prefix)|> 
    in_period({{ date_col }}, period_11, overlap, prefix)|> 
    in_period({{ date_col }}, period_12, overlap, prefix)
}

oud_hillary <- 
  fselect(hillary, BENE_ID, oud_hillary_dt) |> 
  left_join(cohort) |> 
  filter(oud_hillary_dt %within% interval(first_treatment_dt, first_treatment_dt + days(365))) |> 
  roworder(BENE_ID, oud_hillary_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, oud_hillary_dt) |> 
  add_all_periods(cohort, y = _, oud_hillary_dt, FALSE, "hillary") |> 
  fmutate(hillary_period_exposure = 
            fifelse(oud_hillary_dt %within% interval(first_treatment_dt, first_treatment_dt), 1, 0)) |> 
  select(BENE_ID, hillary_period_exposure, starts_with("hillary_period")) |> 
  fmutate(across(hillary_period_exposure:hillary_period_12, replace_na))

oud_poison <- 
  fselect(poison, BENE_ID, oud_poison_dt) |> 
  left_join(cohort) |> 
  filter(oud_poison_dt %within% interval(first_treatment_dt, first_treatment_dt + days(365))) |> 
  roworder(BENE_ID, oud_poison_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, oud_poison_dt) |> 
  add_all_periods(cohort, y = _, oud_poison_dt, FALSE, "poison") |> 
  fmutate(poison_period_exposure = 
            fifelse(oud_poison_dt %within% interval(first_treatment_dt, first_treatment_dt), 1, 0)) |> 
  select(BENE_ID, poison_period_exposure, starts_with("poison_period")) |> 
  fmutate(across(poison_period_exposure:poison_period_12, replace_na))

oud_bup <- 
  mutate(bup, moud_period = interval(moud_start_dt, moud_end_dt)) |> 
  fselect(BENE_ID, moud_period, moud_start_dt, moud_end_dt) |> 
  left_join(cohort) |> 
  filter(int_overlaps(moud_period, interval(first_treatment_dt, first_treatment_dt + days(365)))) |> 
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, moud_period) |> 
  add_all_periods(cohort, y = _, moud_period, TRUE, "bup") |> 
  fmutate(bup_period_exposure = 
            fifelse(moud_period %within% interval(first_treatment_dt, first_treatment_dt), 1, 0)) |> 
  select(BENE_ID, bup_period_exposure, starts_with("bup_period")) |> 
  fmutate(across(bup_period_exposure:bup_period_12, replace_na))

oud_methadone <- 
  mutate(methadone, moud_period = interval(moud_start_dt, moud_end_dt)) |> 
  fselect(BENE_ID, moud_period, moud_start_dt, moud_end_dt) |> 
  left_join(cohort) |> 
  filter(int_overlaps(moud_period, interval(first_treatment_dt, first_treatment_dt + days(365)))) |> 
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, moud_period) |> 
  add_all_periods(cohort, y = _, moud_period, TRUE, "methadone") |> 
  fmutate(methadone_period_exposure = 
            fifelse(moud_period %within% interval(first_treatment_dt, first_treatment_dt), 1, 0)) |> 
  select(BENE_ID, methadone_period_exposure, starts_with("methadone_period")) |> 
  fmutate(across(methadone_period_exposure:methadone_period_12, replace_na))

oud_nal <- 
  mutate(nal, moud_period = interval(moud_start_dt, moud_end_dt)) |> 
  fselect(BENE_ID, moud_period, moud_start_dt, moud_end_dt) |> 
  left_join(cohort) |> 
  filter(int_overlaps(moud_period, interval(first_treatment_dt, first_treatment_dt + days(365)))) |> 
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |> 
  group_by(BENE_ID) |> 
  filter(row_number() == 1) |> 
  ungroup() |> 
  fselect(BENE_ID, moud_period) |> 
  add_all_periods(cohort, y = _, moud_period, TRUE, "nal") |> 
  fmutate(nal_period_exposure = 
            fifelse(moud_period %within% interval(first_treatment_dt, first_treatment_dt), 1, 0)) |> 
  select(BENE_ID, nal_period_exposure, starts_with("nal_period")) |> 
  fmutate(across(nal_period_exposure:nal_period_12, replace_na))

in_period_misuse <- function(period) {
  fselect(opioids, BENE_ID, RX_FILL_DT, PRSCRBNG_PRVDR_NPI, DSPNSNG_PRVDR_NPI, DAYS_SUPPLY) |> 
    left_join(select(cohort, BENE_ID, first_treatment_dt, {{ period }})) |> 
    filter(RX_FILL_DT %within% {{ period }}) |> 
    fgroup_by(BENE_ID) |> 
    fsummarise(distinct_providers = n_distinct(PRSCRBNG_PRVDR_NPI), 
               distinct_dispensers = n_distinct(DSPNSNG_PRVDR_NPI),
               total_days_supply = sum(DAYS_SUPPLY)) |> 
    mutate(
      score_providers = case_when(
        distinct_providers <= 2 ~ 0,
        distinct_providers <= 4 ~ 1,
        distinct_providers >= 5 ~ 2
      ),
      score_dispensers = case_when(
        distinct_dispensers <= 2 ~ 0,
        distinct_dispensers <= 4 ~ 1,
        distinct_dispensers >= 5 ~ 2
      ),
      score_days_supply = case_when(
        total_days_supply <= 185 ~ 0,
        total_days_supply <= 240 ~ 1,
        total_days_supply > 240 ~ 2,
        is.na(total_days_supply) ~ 0
      ), 
      "misuse_{{ period }}" := as.numeric((score_providers +  score_dispensers + score_days_supply) >= 5)
    ) |> 
    select(BENE_ID, starts_with("misuse"))
}

cohort <- mutate(cohort, period_exposure = interval(first_treatment_dt, first_treatment_dt))

oud_misuse <- 
  list(in_period_misuse(period_exposure), 
       in_period_misuse(period_1),
       in_period_misuse(period_2),
       in_period_misuse(period_3),
       in_period_misuse(period_4),
       in_period_misuse(period_5),
       in_period_misuse(period_6),
       in_period_misuse(period_7),
       in_period_misuse(period_8),
       in_period_misuse(period_9),
       in_period_misuse(period_10),
       in_period_misuse(period_11),
       in_period_misuse(period_12)) |> 
  reduce(left_join) |> 
  right_join(select(cohort, BENE_ID)) |> 
  fmutate(across(misuse_period_exposure:misuse_period_12, replace_na))

# only include hillary + poision in main outcome

oud <- 
  list(
    oud_hillary, 
    oud_poison, 
    oud_bup,
    oud_methadone,
    oud_nal,
    oud_misuse
  ) |> 
  reduce(left_join) |> 
  mutate(oud_period_exposure = if_any(.cols = ends_with("period_exposure"), \(x) x == 1),
         oud_period_1 = if_any(.cols = ends_with("period_1"), \(x) x == 1), 
         oud_period_2 = if_any(.cols = ends_with("period_2"), \(x) x == 1), 
         oud_period_3 = if_any(.cols = ends_with("period_3"), \(x) x == 1), 
         oud_period_4 = if_any(.cols = ends_with("period_4"), \(x) x == 1), 
         oud_period_5 = if_any(.cols = ends_with("period_5"), \(x) x == 1), 
         oud_period_6 = if_any(.cols = ends_with("period_6"), \(x) x == 1), 
         oud_period_7 = if_any(.cols = ends_with("period_7"), \(x) x == 1), 
         oud_period_8 = if_any(.cols = ends_with("period_8"), \(x) x == 1), 
         oud_period_9 = if_any(.cols = ends_with("period_9"), \(x) x == 1), 
         oud_period_10 = if_any(.cols = ends_with("period_10"), \(x) x == 1), 
         oud_period_11 = if_any(.cols = ends_with("period_11"), \(x) x == 1), 
         oud_period_12 = if_any(.cols = ends_with("period_12"), \(x) x == 1), 
         
         across(starts_with("oud_period"), as.numeric)) |> 
  select(BENE_ID, starts_with("oud_period")) |> 
  lmtp::event_locf(paste0("oud_period_", 1:12))

oud_hillary <- 
  list(
    oud_hillary
  ) |> 
  reduce(left_join) |> 
  mutate(oud_hillary_period_exposure = if_any(.cols = ends_with("period_exposure"), \(x) x == 1),
         oud_hillary_period_1 = if_any(.cols = ends_with("period_1"), \(x) x == 1), 
         oud_hillary_period_2 = if_any(.cols = ends_with("period_2"), \(x) x == 1), 
         oud_hillary_period_3 = if_any(.cols = ends_with("period_3"), \(x) x == 1), 
         oud_hillary_period_4 = if_any(.cols = ends_with("period_4"), \(x) x == 1), 
         oud_hillary_period_5 = if_any(.cols = ends_with("period_5"), \(x) x == 1), 
         oud_hillary_period_6 = if_any(.cols = ends_with("period_6"), \(x) x == 1), 
         oud_hillary_period_7 = if_any(.cols = ends_with("period_7"), \(x) x == 1), 
         oud_hillary_period_8 = if_any(.cols = ends_with("period_8"), \(x) x == 1), 
         oud_hillary_period_9 = if_any(.cols = ends_with("period_9"), \(x) x == 1), 
         oud_hillary_period_10 = if_any(.cols = ends_with("period_10"), \(x) x == 1), 
         oud_hillary_period_11 = if_any(.cols = ends_with("period_11"), \(x) x == 1), 
         oud_hillary_period_12 = if_any(.cols = ends_with("period_12"), \(x) x == 1), 
         
         across(starts_with("oud_hillary_period"), as.numeric)) |> 
  select(BENE_ID, starts_with("oud_hillary_period")) |> 
  lmtp::event_locf(paste0("oud_hillary_period_", 1:12))

write_data(oud, "msk_washout_continuous_enrollment_oud_outcomes.fst", file.path(drv_root, "outcome"))
write_data(oud_hillary, "msk_washout_continuous_enrollment_oud_hillary_outcomes.fst", file.path(drv_root, "outcome"))
