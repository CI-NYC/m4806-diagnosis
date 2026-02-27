library(tidyverse)
library(fst)
library(lubridate)
library(collapse)


source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("inclusion_exclusion_cohort_with_outcomes.fst", file.path(drv_root, "final")) |>
  as.data.table()

# getting opioid category data -------------------------------------------------
days <- load_data("days_supply.fst", file.path(drv_root, "outcome"))
mme <- load_data("max_daily_dose_mme.fst", file.path(drv_root, "outcome"))

merged <- days |>
  left_join(mme, by= "BENE_ID")

opioid_category <- function(days, mme) {
  case_when(
    days == 0               ~ "No_opioid",
    mme <= 50 & days <= 7   ~ "Opioid <=7days <=50mme",
    mme <= 50 & days > 7    ~ "Opioid >7days <=50mme",
    mme > 50                ~ "Opioid >50mme"
  )
}

opioid_category_df <- map_dfr(1:12, function(month) {
  days_col <- paste0("mo", month, "_days_supply")
  mme_col  <- paste0("mo", month, "_max_daily_dose_mme")
  
  merged |>
    fmutate(
      treatment_name = opioid_category(
        get(days_col),
        get(mme_col)
      )
    ) |>
    fselect(BENE_ID, treatment_name) |>
    mutate(month = month)
}) |>
  filter(BENE_ID %in% cohort$BENE_ID,
         !treatment_name == "No_opioid")

# other treatments -------------------------------------------------------------
nop_rx_dts <- load_data("nonopioid_rx_dts.fst", file.path(drv_root, "treatment"))
nonpharma_dts <- load_data("nonpharma_dts.fst", file.path(drv_root, "treatment"))

treatments <- rbind(nop_rx_dts, nonpharma_dts) |> 
  filter(BENE_ID %in% cohort$BENE_ID)

# Month 1
cohort_month_1 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt,
             first_treatment_dt + days(29))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%
  rbind(opioid_category_df |> filter(month == 1) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) 

cohort_month_1_risk_level <- cohort_month_1 |>
  mutate(risk = as.integer(if_any(c(`Opioid >50mme`, `Opioid >7days <=50mme`, `Benzodiazepine`, `Gabapentin`))))

cohort_month_later <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(30),
             first_treatment_dt + days(359))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%
  rbind(opioid_category_df |> filter(month == 1) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) 

cohort_month_later_risk_level <- cohort_month_later |>
  mutate(risk = as.integer(if_any(c(`Opioid >50mme`, `Opioid >7days <=50mme`, `Benzodiazepine`, `Gabapentin`))))

merged <- cohort_month_1_risk_level |>
  filter(risk == 0) |>
  inner_join(cohort_month_later_risk_level, by= "BENE_ID") |>
  summarise(risk.y-risk.x)

table(merged)
# risk.y - risk.x
# 0    1 
# 3925 1899 

# conclusion: ~ 48% of those on low risk treatment in month 1 who continue to be treated take up higher risk treatments
# This is also 1899/12745 = 15% of the initial cohort; 15% of those diagnosed and treated for M4806 are started on lower risk treatments, then escalate to higher treatments