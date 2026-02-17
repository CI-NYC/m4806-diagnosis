# -------------------------------------
# Script: surgeries and IP admissions
# Author: Anton Hung
# Purpose: Identifying inpatient and surgeries that may potentially coincide with ED visits
#           The intention is to remove ED visits if they occur within 1 week of 
#           a surgery or inpatient visit, because these are likely not related to
#           an ongoing pain issue.
# Notes:
# -------------------------------------

library(dplyr)
library(data.table)
library(arrow)
library(yaml)
library(lubridate)
library(tidyverse)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

otl <- open_otl()
iph <- open_iph()

# surgery claims
variable = "Surgery"
codes <- read_yaml("~/medicaid/undertreated-pain/data/public/surgery_codes.yml")
codes <- c(names(codes[[variable]]$CPT),
           names(codes[[variable]]$ICD10))

# Filter OTL to claims codes
otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD")
otl <- select(otl, all_of(otl_vars)) |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter(LINE_PRCDR_CD %in% codes) |>
  filter(!is.na(BENE_ID)) |>
  mutate(LINE_SRVC_END_DT = case_when(is.na(LINE_SRVC_END_DT) ~ LINE_SRVC_BGN_DT, TRUE ~ LINE_SRVC_END_DT)) |>
  collect()

otl <- otl |>
  group_by(CLM_ID) |>
  mutate(SRVC_BGN_DT = min(LINE_SRVC_BGN_DT),
         SRVC_END_DT = max(LINE_SRVC_END_DT)) |> # combining all procedure codes into a list
  # slice(1) |>
  ungroup() |>
  select(BENE_ID, CLM_ID, SRVC_BGN_DT, SRVC_END_DT)




############# Inpatient the week before and the week after

iph_vars <- c("BENE_ID", "CLM_ID", "SRVC_BGN_DT", "SRVC_END_DT", "ADMSN_DT", "DSCHRG_DT", "PRCDR_CD_1","PRCDR_CD_2", "PRCDR_CD_3", "PRCDR_CD_4", "PRCDR_CD_5", "PRCDR_CD_6")
# "PRCDR_CD_DT_1", "PRCDR_CD_DT_2", "PRCDR_CD_DT_3", "PRCDR_CD_DT_4", "PRCDR_CD_DT_5", "PRCDR_CD_DT_6")

iph <- select(iph, all_of(iph_vars)) |>
  filter(BENE_ID %in% cohort$BENE_ID) |>
  # filter(if_any(starts_with("PRCDR_CD"),  ~. %in% codes)) |>
  filter(!is.na(BENE_ID)) |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |>
  collect() |>
  mutate(SRVC_BGN_DT = fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT),
         maxdate = pmax(SRVC_BGN_DT, SRVC_END_DT, ADMSN_DT, DSCHRG_DT,
                        # PRCDR_CD_DT_1, PRCDR_CD_DT_2, PRCDR_CD_DT_3, PRCDR_CD_DT_4, PRCDR_CD_DT_5, PRCDR_CD_DT_6,
                        na.rm = TRUE),
         SRVC_END_DT = fifelse(SRVC_BGN_DT > SRVC_END_DT, maxdate, SRVC_END_DT)) |>
  select(BENE_ID, CLM_ID, SRVC_BGN_DT, SRVC_END_DT)

# iph <- iph |>
#   pivot_longer(cols = starts_with("PRCDR_CD"),
#                names_to = "PRCDR_num",
#                values_to = "PRCDR_CD") |>
#   filter(PRCDR_CD %in% codes) |>
#   group_by(CLM_ID) |>
#   mutate(PRCDR_CD = list(PRCDR_CD)) |>
#   slice(1) |>
#   ungroup() |>
#   select(BENE_ID, CLM_ID, SRVC_BGN_DT, SRVC_END_DT, PRCDR_CD)


claims <- rbind(otl, iph)

write_data(claims, "procedures_and_IP.rds", file.path(drv_root, "outcome"))
