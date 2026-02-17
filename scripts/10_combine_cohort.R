# -------------------------------------
# Script: combine_cohort
# Author: Anton Hung
# Purpose: merge comorbidity baseline covariates to data
# Notes:
# -------------------------------------
library(tidyverse)
source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("msk_cohort.fst", file.path(drv_root, "final"))

adhd <- readRDS(file.path(drv_root, "baseline/adhd.rds"))
anxiety <- readRDS(file.path(drv_root, "baseline/anxiety.rds"))
bipolar <- readRDS(file.path(drv_root, "baseline/bipolar.rds"))
depression <- readRDS(file.path(drv_root, "baseline/depression.rds"))
mental_ill <- readRDS(file.path(drv_root, "baseline/mental_ill.rds"))
counseling <- load_data("counseling.fst", file.path(drv_root, "baseline"))
baseline_ip_op <- load_data("baseline_ip_op.fst", file.path(drv_root, "baseline"))
num_ED_visits <- load_data("cohort_num_ED_visits.fst", file.path(drv_root, "baseline"))
substance_use_disorder <- load_data("substance_use_disorder_washout_cal.fst", file.path(drv_root, "baseline"))

# # make dgns_cd wide (dummy variables)
# dgns_cd <- load_data("msk_washout_dts.fst", file.path(drv_root, "exclusion")) |>
#   select(BENE_ID, dgns_cd) |>
#   filter(BENE_ID %in% cohort$BENE_ID) |>
#   mutate(value = 1) |>
#   pivot_wider(names_from = dgns_cd, values_from = value, values_fill = 0, names_prefix = "dgns_cd_")


cohort_MH_joined <- cohort |>
  left_join(adhd |> select(BENE_ID, adhd_washout_6mos_cal) |> rename("adhd_washout_cal" = "adhd_washout_6mos_cal")) |>
  left_join(anxiety |> select(BENE_ID, anxiety_washout_6mos_cal) |> rename("anxiety_washout_cal" = "anxiety_washout_6mos_cal")) |>
  left_join(bipolar |> select(BENE_ID, bipolar_washout_6mos_cal) |> rename("bipolar_washout_cal" = "bipolar_washout_6mos_cal")) |>
  left_join(depression |> select(BENE_ID, depression_washout_6mos_cal) |> rename("depression_washout_cal" = "depression_washout_6mos_cal")) |>
  left_join(mental_ill |> select(BENE_ID, mental_ill_washout_6mos_cal) |> rename("mental_ill_washout_cal" = "mental_ill_washout_6mos_cal")) |>
  left_join(counseling) |>
  left_join(substance_use_disorder) |>
  left_join(baseline_ip_op) |>
  left_join(num_ED_visits) |>
  select(BENE_ID, 
         ends_with("dt", ignore.case = FALSE), 
         starts_with("dem"),
         ends_with("_washout_cal"),
         starts_with("mo"),
         starts_with("oud"),
         starts_with("cens"),
         starts_with("outcome")
  )

write_data(cohort_MH_joined, "msk_cohort_with_MH.fst", file.path(drv_root, "final"))
