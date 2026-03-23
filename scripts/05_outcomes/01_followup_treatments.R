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
    fsubset(int_overlaps(
      interval(treatment_start_dt, treatment_end_dt),
      interval(first_treatment_dt + days(day_start), 
               first_treatment_dt + days(day_end))
    )) |>
    fselect(BENE_ID, treatment_name) |>
    funique()
  
  # 2. Create the wide format and combinations
  summary_df <- treatments_in_month |>
    group_by(BENE_ID) |>
    summarise(
      treatment_combination = str_flatten(sort(unique(treatment_name)), collapse = ", "),
      treatment_name = treatment_name
    ) |>
    fmutate(present = 1) |>
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

final <- list(opioid_category,
              get_monthly_treatments(cohort, treatments, 1),
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
  reduce(left_join) |>
  as.data.table()

# 3. Modification to evaluation of treatments: create a single time-varying binary treatment.
#     This will be calculated as so:
#     The lack of any of the higher risk treatments (”Opioid >7days ≤50mme”, 
#     ”Opioid >50mme”, ”Gabapentin”, ”Benzodiazepine”) implies the presence of 
#     one or more of the remaining lower risk treatments. When this occurs, 
#     indicate this with a 0 for our new variable. On the other hand, if a
#     high risk treatment exists, indicate this with a 1.

# new column "mo{}_highrisk" records whether any of the 4 high risk treatments are present in a month
# new column "mo{}_treated" records whether any treatment at all is present in a month
final[, c(paste0("mo", seq_len(num_periods), "_highrisk"), 
          paste0("mo", seq_len(num_periods), "_treated")) :=
        {
          # first vector: any high-risk treatment
          highrisk_col <- lapply(seq_len(num_periods), function(i) {
            high_risk_treatment <- paste0("mo", i, "_",
                                          c("Opioid_g50mme",
                                            "Opioid_g7days_le50mme",
                                            "Benzodiazepine",
                                            "Gabapentin"))
            as.integer(rowSums(.SD[, high_risk_treatment, with = FALSE],
                               na.rm = TRUE) > 0)
          })
          
          # second vector: any treatment at all
          treated_col <- lapply(seq_len(num_periods), function(i) {
            any_treatment <- grep(paste0("^mo", i, "_"), names(final), value = TRUE)
            any_treatment <- any_treatment[!grepl("treatment_combination$", any_treatment)]
            as.integer(rowSums(.SD[, any_treatment, with = FALSE],
                               na.rm = TRUE) > 0)
          })
          
          c(highrisk_col, treated_col)
        }
]


# # keep only these columns
# final <- final[,
#   c("BENE_ID", 
#     paste0("mo",seq_len(num_periods),"_highrisk"), 
#     paste0("mo",seq_len(num_periods),"_treated")), with=F
# ]

# add opioid data - just to not lose this information
final <- list(final,
              days,
              mme) |>
  reduce(left_join)

# numeric_sums <- colSums(select(final, where(is.numeric), -"BENE_ID"), na.rm = TRUE)

write_data(final, "followup_treatments.fst", file.path(drv_root, "outcome"))
