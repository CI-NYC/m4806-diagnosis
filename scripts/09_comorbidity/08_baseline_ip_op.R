# -------------------------------------
# Script: baseline_ip_op
# Author: Anton Hung
# Updated:
# Purpose: create confounders for the number of inpatient hospitalizations,
#          outpatient visits during the washout period
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

# Read in OTH and IPH as arrow datsets ----------------------------------

oth <- open_oth()
iph <- open_iph()
rxl <- open_rxl()

# read in cohort dates file
dts_cohorts <- load_data("msk_cohort.fst", file.path(drv_root, "final"))
ED_visits <- load_data("ED_visits_cleaned_with_procedures_and_inpatients_excluded.fst", file.path(drv_root, "outcome"))

#https://resdac.org/sites/datadocumentation.resdac.org/files/2021-01/5011_Identifying_IP_Stays.pdf
# inpatient_cds <- c("001", "060", "084", "086", "090", "091", "092", "093")
# inpatient_POS_CD <- c(21, 31:34, 51, 54, 61)
# outpatient_TOS_CD <- c("002", "003", "028", "060", "061", "014", "049")
ed_visit_cds <- c(paste0("045", 0:9), "0981", # Emergency department
                  "0526", "0516" # Urgent care
)

# inpatient hospitalizations --------------------------------------------------------------------------------
iph <- iph |>
  select(BENE_ID, CLM_ID, SRVC_BGN_DT, SRVC_END_DT) |>
  # filter(!is.na(TOS_CD)) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect() |>
  distinct(BENE_ID, SRVC_BGN_DT, .keep_all = T) |>
  inner_join(dts_cohorts |> select(BENE_ID, washout_start_dt, washout_end_dt)) |>
  filter(SRVC_BGN_DT %within% interval(washout_start_dt, washout_end_dt))

# count number of inpatient hospitalizations during washout period
num_iph_washout_cal <-
  iph |>
  group_by(BENE_ID) |>
  summarise(num_iph_washout_cal = n_distinct(SRVC_BGN_DT),
            .groups = "drop")

# outpatient visits ------------------------------------------------------------
icd_codes_to_check_oth <-
  oth |>
  filter(BENE_ID %in% dts_cohorts$BENE_ID,
         !CLM_ID %in% ED_visits$ed_visit_ID) |>
  select(BENE_ID, SRVC_BGN_DT) |>
  collect()

# obtain the date for all outpatient visits within washout period
all_oth_icds_in_washout_cal <-
  icd_codes_to_check_oth |>
  inner_join(dts_cohorts |> select(BENE_ID, washout_start_dt, washout_end_dt)) |>
  filter(SRVC_BGN_DT %within% interval(washout_start_dt, washout_end_dt))  

# count number of outpatient visits during washout period
num_oth_washout_cal <-
  all_oth_icds_in_washout_cal |>
  distinct(BENE_ID, SRVC_BGN_DT) |>
  group_by(BENE_ID) |>
  summarise(num_oth_washout_cal = n(), .groups = "drop")

# # pharmacy claims --------------------------------------------------------
# rxl_washout_cal <- 
#   rxl |>
#   select(BENE_ID, RX_FILL_DT) |>
#   collect() |>
#   inner_join(dts_cohorts |> select(BENE_ID, washout_start_dt, washout_end_dt)) |>
#   filter(RX_FILL_DT %within% interval(washout_start_dt, washout_end_dt))
# 
# num_rxl_washout_cal <-
#   rxl_washout_cal |>
#   distinct(BENE_ID, RX_FILL_DT) |>
#   group_by(BENE_ID) |>
#   summarise(num_rxl_washout_cal = n(), .groups = "drop")
# 
cohort <- dts_cohorts |>
  select(BENE_ID) |>
  left_join(num_iph_washout_cal, by = "BENE_ID") |>
  left_join(num_oth_washout_cal, by = "BENE_ID") |>
  # left_join(num_rxl_washout_cal, by = "BENE_ID") |>
  mutate(num_iph_washout_cal = replace_na(num_iph_washout_cal, 0),
         num_oth_washout_cal = replace_na(num_oth_washout_cal, 0)
         # num_rxl_washout_cal = replace_na(num_rxl_washout_cal, 0)
         )

# cap at a reasonable number (99th percentile)
cohort <- cohort |>
  mutate(num_iph_washout_cal = ifelse(num_iph_washout_cal > 1, 1, num_iph_washout_cal),
         num_oth_washout_cal = ifelse(num_oth_washout_cal > 70, 70, num_oth_washout_cal)
         # num_rxl_washout_cal = ifelse(num_rxl_washout_cal > 40, 40, num_rxl_washout_cal)
         )

write_data(cohort, "baseline_ip_op.fst", file.path(drv_root, "baseline"))
