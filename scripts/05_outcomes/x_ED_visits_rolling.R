# -------------------------------------
# Script: ED visits rolling windows
# Author: Anton Hung
# Purpose: Computing the outcome "2+ ED visits in any 30-day window"
#           This is done by creating 30-day rolling windows, where the start dates
#           for each window are separated by 1 week. This makes 62 windows. If any
#           of these windows captures 2 ED visits, then the outcome for this patient is positive.
# Notes:
# -------------------------------------
library(tidyverse)
library(fst)
library(collapse)
library(lubridate)
library(data.table)
library(foreach)
library(future)
library(furrr)
library(ggalluvial)
library(doParallel)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  select(BENE_ID, first_treatment_dt)

# load component files ----------------------------------------------------

ed_visits <- load_data("ED_visits_cleaned_with_procedures_and_inpatients_excluded.fst", file.path(drv_root, "outcome"))

ed_visits <- 
  fselect(ed_visits, BENE_ID, ED_visit_dt) |> 
  left_join(cohort) |>
  filter(ED_visit_dt %within% interval(exposure_period_end_dt, first_treatment_dt + days(365))) |> 
  roworder(BENE_ID, ED_visit_dt)


# finding whether there are ever 2 ed_visit_dt observations within 30 days
# windows roll by week. 65 weeks to 15 months
rolling_windows <- function(pain_cat_df, week_start){
  print(paste(week_start, Sys.time()))
  
  relevant_ed_dts <-
    pain_cat_df |>
    # keep only codes within the 6 month window of interest
    mutate(start_week = exposure_period_end_dt + weeks(week_start) + days(1),
           end_week = exposure_period_end_dt + weeks(week_start) + days(31)) |>
    # then, filter the diagnosis codes to only contain those filled within the relevant time frame
    filter(ED_visit_dt %within% interval(start_week, end_week)) |>
    group_by(BENE_ID) |>
    add_count() |> # add number of dg codes within this window per beneficiary
    filter(n > 1) |> # only keep codes that show up more than once 
    ungroup()
  
  ed_visit_per_month <-
    relevant_ed_dts |>
    select(BENE_ID) |>
    distinct() |>
    mutate(week = week_start)
  
  ed_visit_per_month[["ed_visit"]] <- 1
  
  saveRDS(ed_visit_per_month, paste0(drv_root,"/outcome/tmp_ed_visit/ed_visit_pieces/week_", week_start, ".rds"))
  
  print(paste(week_start, "COMPLETE", Sys.time()))
  
  return(ed_visit_per_month)
}

# options(future.globals.maxSize= 3000000000)
# future_map(0:61, ~rolling_windows(ed_visits,  .x))

dir <- file.path(drv_root, "outcome/tmp_ed_visit/ed_visit_pieces/")
files_all <- list.files(dir)

overall_pain_by_month <- function(week_number){
  month_files <- files_all[which(str_detect(files_all, paste0("week_", week_number, ".rds")))]
  month_df <- map(month_files, ~read_rds(paste0(dir, .x))) |>
    reduce(full_join)  |>
    mutate(across(where(is.numeric), ~replace_na(.x, 0))) |>
    mutate(ed_visit_any = 1) |> # only in this data set if they have chronic pain for that month
    select(BENE_ID, week, ed_visit_any)
  return(month_df)
}

ed_visit_all_weeks <- map_dfr(0:61, overall_pain_by_month)
saveRDS(ed_visit_all_weeks, file.path(drv_root, "outcome/tmp_ed_visit/ed_visit_all_weeks.rds"))

ed_visit_all_weeks <- read_rds(file.path(drv_root, "outcome/tmp_ed_visit/ed_visit_all_weeks.rds"))

ed_visit_wide <- pivot_wider(ed_visit_all_weeks,
                                 id_cols = BENE_ID,
                                 names_from = week,
                                 names_prefix = "ed_visit_week_",
                                 values_from = ed_visit_any,
                                 values_fill = 0) |>
  mutate(ed_visit_n_weeks = rowSums(across(where(is.numeric))))

ed_visit_wide <- ed_visit_wide  |>
  mutate(outcome_ed_visit = as.numeric(ed_visit_n_weeks>0)) |>
  select(BENE_ID, outcome_ed_visit)

cohort <- cohort |>
  left_join(ed_visit_wide) |>
  mutate(outcome_ed_visit = replace_na(outcome_ed_visit, 0)) |>
  select(BENE_ID, outcome_ed_visit)

saveRDS(cohort, file.path(drv_root, "outcome/tmp_ed_visit/ed_visit_outcome.rds"))