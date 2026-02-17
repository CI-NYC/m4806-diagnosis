# -------------------------------------
# Script: ED_visits_find_all
# Author: Anton Hung
# Purpose: aggregating  all ED visits in the other services file which have a pain-related
#           diagnosis (using Kat Hoffman's pain codes).
# Notes:
# -------------------------------------

library(tidyverse)
library(data.table)
library(arrow)
library(yaml)

# claims data
source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

# base cohort
cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

# Get ED visits first -- pain diagnoses later ---------------------------------
revenue_cds <- c(paste0("045", 0:9), "0981", # Emergency department
                 "0526", "0516" # Urgent care
                 )
procedure_cds <- c("99281","99282","99283","99284","99285","99288", # Emergency department
                   "S9083", "S9088", "99051", "99058", "Y92.532" # Urgent care
                   )

# Exclude procedure codes that are related to lab-only visits to the ED
excluded_cpt_cds <- c(36400:36415, "G0001", 43200:43272, 45300:45387, 70010:79999, 80048:89399, 93000:93278)

# Read in OTL (Other services line)
otl <- open_otl()

start_dt <- as.Date("2016-01-01")
end_dt <- as.Date("2019-12-31")

# Filter OTL to claims codes
otl_vars <- c("BENE_ID", "CLM_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD", "REV_CNTR_CD")
otl <- 
  select(otl, any_of(otl_vars)) |> 
  filter(BENE_ID %in% cohort$BENE_ID) |>
  filter((REV_CNTR_CD %in% revenue_cds |
         LINE_PRCDR_CD %in% procedure_cds),
         !LINE_PRCDR_CD %in% excluded_cpt_cds) |>  
  collect() |> 
  as.data.table()

otl[, SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT)]

otl <- otl[LINE_SRVC_BGN_DT %within% interval(start_dt, end_dt), 
                   .(BENE_ID, CLM_ID, LINE_SRVC_BGN_DT)]

otl <- otl |>
  distinct(CLM_ID, .keep_all=TRUE)





# Pain diagnosis ---------------------------------------------------------------
chronic_pain_icds <- read.csv("~/medicaid/pain-severity/input/chronic_pain_icd10_20230216.csv") |>
  filter(CRITERIA == "Inclusion")

oth <- open_oth()

dg <- oth |>
  select(BENE_ID, CLM_ID, SRVC_BGN_DT, SRVC_END_DT, DGNS_CD_1, DGNS_CD_2) |>
  filter(BENE_ID %in% cohort$BENE_ID,
         if_any(starts_with("DGNS_CD_"), ~. %in% c(chronic_pain_icds$ICD9_OR_10, 
                                                   "R109", "R1084"))) |> # additional codes for abdominal pain
  collect() |>
  as.data.table()

dg <- dg |>
  mutate(dgns_cd = fifelse(DGNS_CD_1 %in% chronic_pain_icds$ICD9_OR_10, DGNS_CD_1, DGNS_CD_2)) |>
  left_join(select(chronic_pain_icds, ICD9_OR_10, PAIN_CAT), by=c("dgns_cd" = "ICD9_OR_10"))

dg <- dg[, .SD[1], by = CLM_ID]


cohort <- dg |>
  inner_join(otl, c("BENE_ID","CLM_ID")) |>
  as.data.table()


write_data(cohort, "ED_visits_unclean.fst", file.path(drv_root, "outcome"))