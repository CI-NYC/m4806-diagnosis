library(tidyverse)
library(lubridate)
library(collapse)
library(data.table)

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

# define which treatments to show in plot
target_cols <- c("Opioid <=7days <=50mme", "Opioid >7days <=50mme",
                 "Opioid >50mme", "Anti-inflammatory", "Muscle relaxant", 
                 "Gabapentin", "Benzodiazepine", "Physical therapy")



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

BENE_ID_safe <- cohort_month_1 |>
  filter(!Gabapentin,
         !Benzodiazepine) |>
  pull(BENE_ID)

evaluate_month <- function(m) {
  start_offset <- (m - 1) * 30
  end_offset   <- start_offset + 29
  
  cohort |>
    filter(BENE_ID %in% BENE_ID_safe) |>
    left_join(treatments, by = "BENE_ID") |>
    filter(int_overlaps(
      interval(treatment_start_dt, treatment_end_dt),
      interval(first_treatment_dt + days(start_offset),
               first_treatment_dt + days(end_offset))
    )) |>
    select(BENE_ID, treatment_name) |>
    distinct() |>
    bind_rows(opioid_category_df |> filter(month == m) |> select(-month)) |>
    mutate(present = 1) |>
    pivot_wider(
      id_cols = BENE_ID,
      names_from = treatment_name,
      values_from = present,
      values_fill = list(present = 0)
    ) |>
    select(Benzodiazepine, Gabapentin) |>
    summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")
}



df <- map_dfr(2:12, ~ evaluate_month(.x) |> mutate(month = .x)) |>
  pivot_longer(
    cols      = -month,
    names_to  = "treatment",
    values_to = "percent"
  )

p1<-ggplot(df, aes(x=month, y=percent, colour=treatment)) +
  geom_line(size=1) +
  geom_point(size=4) +
  labs(x="Month",
       y="Patients (%)",
       colour="Treatment") +
  scale_x_discrete(limits=as.character(1:12)) +
  theme_light()


ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/02_escalating_treatments/treatment_percentage.pdf", p1, width=10, height=5.5)
