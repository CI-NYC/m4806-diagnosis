# -------------------------------------
# Script: 08_non_pharmaceuticals.R
# Author: Anton Hung
# Updated:
# Purpose: create variable for counsleing during the washout period
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

otl <- open_otl()

# Read in cohort and dates
dts_cohorts <- load_data("msk_cohort.fst", file.path(drv_root, "final"))

codes <- read_yaml("~/medicaid/M4806-diagnosis-cohort/data/public/mediator_codes.yml")

codes <- c(names(codes$Counseling[["CPT"]]),
           names(codes$Counseling[["HCPC"]]),
           names(codes$Counseling[["ICD10"]]))

claims_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |> 
  filter(LINE_PRCDR_CD %in% codes) |>
  collect()

setDT(claims)
setkey(claims, BENE_ID)

claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                     LINE_SRVC_END_DT, 
                                     LINE_SRVC_BGN_DT)]

# Inner join with cohort 
claims <- unique(merge(claims, dts_cohorts, by = "BENE_ID"))

# Filter to claims within mediator time-frame
claims <- claims[LINE_SRVC_BGN_DT %within% interval(washout_start_dt, 
                                                    washout_end_dt), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, LINE_PRCDR_CD)]

baseline_counseling <- 
  dts_cohorts |>
  mutate(counseling_washout_cal = as.numeric(BENE_ID %in% claims$BENE_ID)) |>
  select(BENE_ID, counseling_washout_cal)

# write_data(unique(treatments_dts), "nonpharma_dts.fst", file.path(drv_root, "treatment"))
write_data(baseline_counseling, "counseling.fst", file.path(drv_root, "baseline"))