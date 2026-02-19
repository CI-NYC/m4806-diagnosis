library(tidyverse)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)


source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion")) |>
  as.data.table()

days <- load_data("days_supply.fst", file.path(drv_root, "outcome"))
mme <- load_data("max_daily_dose_mme.fst", file.path(drv_root, "outcome"))
opioid_category <- load_data("opioid_category.fst", file.path(drv_root, "outcome"))
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment"))
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment")) |>
  filter(!treatment_name == "Acupuncture")

treatments <- rbind(nop_rx_dts, nonpharma_dts) |> 
  filter(BENE_ID %in% cohort$BENE_ID)


get_monthly_treatments <- function(cohort_df, treatment_df, month_idx) {
  
  # Calculate window boundaries based on 30-day increments
  # Month 1: 0 to 29 | Month 2: 30 to 59, etc.
  day_start <- (month_idx - 1) * 30
  day_end   <- (month_idx * 30) - 1
  
  # 1. Filter for overlaps in the specific window
  treatments_in_month <- cohort_df |>
    left_join(treatment_df, by = "BENE_ID") |>
    filter(int_overlaps(
      interval(treatment_start_dt, treatment_end_dt),
      interval(first_treatment_dt + days(day_start), 
               first_treatment_dt + days(day_end))
    )) |>
    select(BENE_ID, treatment_name) |>
    distinct()
  
  # 2. Create the wide format and combinations
  summary_df <- treatments_in_month |>
    group_by(BENE_ID) |>
    summarise(
      treatment_combination = str_flatten(sort(unique(treatment_name)), collapse = ", "),
      treatment_name = treatment_name
    ) |>
    mutate(present = 1) |>
    pivot_wider(
      names_from = treatment_name,
      values_from = present,
      values_fill = 0
    ) |>
    rename_with(
      .fn = ~ paste0("mo", month_idx, "_", .x), 
      .cols = -BENE_ID
    ) |>
    ungroup()
  
  return(summary_df)
}

final <- list(get_monthly_treatments(cohort, treatments, 1),
              get_monthly_treatments(cohort, treatments, 2),
              get_monthly_treatments(cohort, treatments, 3),
              get_monthly_treatments(cohort, treatments, 4),
              get_monthly_treatments(cohort, treatments, 5),
              get_monthly_treatments(cohort, treatments, 6),
              get_monthly_treatments(cohort, treatments, 7),
              get_monthly_treatments(cohort, treatments, 8),
              get_monthly_treatments(cohort, treatments, 9),
              get_monthly_treatments(cohort, treatments, 10),
              get_monthly_treatments(cohort, treatments, 11),
              get_monthly_treatments(cohort, treatments, 12)) |>
  reduce(join, how="left")

final <- list(days,
              mme,
              opioid_category) |>
  reduce(left_join) |>
  left_join(final)

# numeric_sums <- colSums(select(final, where(is.numeric), -"BENE_ID"), na.rm = TRUE)

write_data(final, "followup_treatments.fst", file.path(drv_root, "outcome"))
