# -------------------------------------
# Script: opioid_categories
# Author: Anton Hung
# Purpose: using the monthly MME and days supply numbers, determine the category of opioid
# Notes: long-duration-low-dose (>7 days, <50 mme), short-duration-low-dose (<=7 days, <50 mme), or high dose (> 50 mme)
# -------------------------------------

library(tidyverse)
library(glue)
library(fst)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

days <- load_data("days_supply.fst", file.path(drv_root, "outcome"))

mme <- load_data("max_daily_dose_mme.fst", file.path(drv_root, "outcome"))

merged <- days |>
  left_join(mme, by= "BENE_ID")

opioid_category <- function(days, mme) {
  case_when(
    days == 0               ~ "No_opioid",
    mme <= 50 & days <= 7   ~ "Opioid_le7days_le50mme",
    mme <= 50 & days > 7    ~ "Opioid_g7days_le50mme",
    mme > 50                ~ "Opioid_g50mme"
  )
}

opioid_category_df <- map_dfr(1:12, function(month) {

  days_col <- paste0("mo", month, "_days_supply")
  mme_col  <- paste0("mo", month, "_max_daily_dose_mme")

  merged |>
    fmutate(
      opioid_category = opioid_category(
        get(days_col),
        get(mme_col)
      )
    ) |>
    fselect(BENE_ID, opioid_category) |>
    mutate(month = month)
})

opioid_category_wide <- opioid_category_df |>
  mutate(present = 1) |>
  pivot_wider(
    id_cols = BENE_ID,
    names_from = c(month, opioid_category),
    values_from = present,
    values_fill = 0,
    names_glue = "mo{month}_{opioid_category}"
  )

opioid_category_wide <- opioid_category_wide |>
  select(-ends_with("No_opioid"))

write_data(opioid_category_wide, "opioid_category.fst", file.path(drv_root, "outcome"))
