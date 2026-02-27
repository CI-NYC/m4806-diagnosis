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
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")


# Month 2
cohort_month_2 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(30),
             first_treatment_dt + days(59))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 2) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")


# Month 3
cohort_month_3 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(60),
             first_treatment_dt + days(89))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 3) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")



# Month 4
cohort_month_4 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(90),
             first_treatment_dt + days(119))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 4) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")



# Month 5
cohort_month_5 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(120),
             first_treatment_dt + days(149))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 5) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")



# Month 6
cohort_month_6 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(150),
             first_treatment_dt + days(179))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 6) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")

# Month 7
cohort_month_7 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(180),
             first_treatment_dt + days(209))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 7) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")

# Month 8
cohort_month_8 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(210),
             first_treatment_dt + days(239))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 8) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")

# Month 9
cohort_month_9 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(240),
             first_treatment_dt + days(269))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 9) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")

# Month 10
cohort_month_10 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(270),
             first_treatment_dt + days(299))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 10) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")

# Month 11
cohort_month_11 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(300),
             first_treatment_dt + days(329))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 11) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")

# Month 12
cohort_month_12 <- cohort |>
  left_join(treatments) |>
  filter(int_overlaps(
    interval(treatment_start_dt, treatment_end_dt),
    interval(first_treatment_dt + days(330),
             first_treatment_dt + days(359))
  )) |>
  select(BENE_ID, treatment_name) %>%
  distinct() %>%    
  rbind(opioid_category_df |> filter(month == 12) |> select(-month)) |>
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = BENE_ID,
    names_from = treatment_name,
    values_from = present,
    values_fill = list(present = 0)
  ) |>
  select(all_of(target_cols)) |>
  summarise(across(everything(), \(x) sum(x)/nrow(cohort) * 100), .groups = "drop")


df <- rbind(cohort_month_1,
            cohort_month_2,
            cohort_month_3,
            cohort_month_4,
            cohort_month_5,
            cohort_month_6,
            cohort_month_7,
            cohort_month_8,
            cohort_month_9,
            cohort_month_10,
            cohort_month_11,
            cohort_month_12) |>
  mutate(month = row_number()) |>
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

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/treatment_percentage.pdf", p1, width=10, height=5.5)
