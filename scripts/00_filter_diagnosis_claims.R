# -------------------------------------
# Script: 00_filter_msk_claims.R
# Author: Anton Hung
# Updated: 
# Purpose: Find all msk claims within the specified date range (specifically, M4806 codes)
#   in the Other Services and Inpatient files
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)
library(fst)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

# Load necessary datasets
oth <- open_oth()
# iph <- open_iph()

codes <- "M4806"
codes_inpatient <- c(13, 21, 32, 24, 55, 31, 09, 41)

start_dt <- as.Date("2016-01-01")
end_dt <- as.Date("2019-12-31")

keep <- c("BENE_ID", 
          "CLM_ID", 
          "POS_CD",
          "SRVC_BGN_DT", 
          "SRVC_END_DT", 
          paste0("DGNS_CD_", 1:10))

oth_msk <- 
  select(oth, any_of(keep)) |> 
  filter(DGNS_CD_1 %in% codes | DGNS_CD_2 %in% codes) |>
  filter(!POS_CD %in% codes_inpatient) |>
  collect() |> 
  as.data.table()

# iph_msk <- 
#   select(iph, any_of(keep)) |> 
#   filter(DGNS_CD_1 %in% codes | 
#            DGNS_CD_2 %in% codes | 
#            DGNS_CD_3 %in% codes | 
#            DGNS_CD_4 %in% codes | 
#            DGNS_CD_5 %in% codes | 
#            DGNS_CD_6 %in% codes | 
#            DGNS_CD_7 %in% codes | 
#            DGNS_CD_8 %in% codes | 
#            DGNS_CD_9 %in% codes | 
#            DGNS_CD_10 %in% codes) |>
#   collect() |> 
#   as.data.table()

oth_msk[, SRVC_BGN_DT := fifelse(is.na(SRVC_BGN_DT), SRVC_END_DT, SRVC_BGN_DT)]

oth_msk <- oth_msk[SRVC_BGN_DT %within% interval(start_dt, end_dt), 
                   .(BENE_ID, CLM_ID, SRVC_BGN_DT, DGNS_CD_1, DGNS_CD_2)]

# iph_msk <- iph_msk[SRVC_BGN_DT %within% interval(start_dt, end_dt), 
#                    .SD, 
#                    .SDcols = c("BENE_ID", "CLM_ID", "SRVC_BGN_DT", paste0("DGNS_CD_", 1:10))]

setkey(oth_msk, BENE_ID, CLM_ID)
oth_msk <- unique(oth_msk, by = c(1, 2))

# setkey(iph_msk, BENE_ID, CLM_ID)
# iph_msk <- unique(iph_msk, by = c(1, 2))

# Remove rows with missing BENE_ID
oth_msk <- oth_msk[!is.na(BENE_ID)]
# iph_msk <- iph_msk[!is.na(BENE_ID)]

# Sort by date and take first claim for each BENE_ID to calculate the earliest possible washout start date
setorder(oth_msk, BENE_ID, SRVC_BGN_DT)
oth_msk[, let(washout_start_dt_possible_earliest = min(SRVC_BGN_DT)), BENE_ID]
oth_msk <- oth_msk[SRVC_BGN_DT == washout_start_dt_possible_earliest]
oth_msk[, let(washout_start_dt_possible_earliest = washout_start_dt_possible_earliest - days(182))]

# # recording what the pain diagnosis code is so we can join with pain categories later
# iph_msk[, dgns_cd := fcoalesce( lapply(.SD, function(x) ifelse(x %in% codes, x, NA_character_)) )]

oth_msk[, let(dgns_cd = ifelse(DGNS_CD_1 %in% codes, DGNS_CD_1, DGNS_CD_2))]

# setorder(iph_msk, BENE_ID, SRVC_BGN_DT)
# iph_msk[, let(washout_start_dt_possible_earliest = min(SRVC_BGN_DT)), BENE_ID]
# iph_msk <- iph_msk[SRVC_BGN_DT == washout_start_dt_possible_earliest]
# iph_msk[, let(washout_start_dt_possible_earliest = washout_start_dt_possible_earliest - days(182))]

oth_msk <- unique(oth_msk[, .(BENE_ID, dgns_cd, washout_start_dt_possible_earliest, SRVC_BGN_DT)], by = 1)
# iph_msk <- unique(iph_msk[, .(BENE_ID, dgns_cd, washout_start_dt_possible_earliest, SRVC_BGN_DT)], by = 1)

msk <- rbindlist(list(oth_msk))
msk[, let(washout_start_dt_possible_earliest_comb = min(washout_start_dt_possible_earliest), dgns_cd = dgns_cd[1L]), BENE_ID]
msk <- msk[washout_start_dt_possible_earliest == washout_start_dt_possible_earliest_comb, .(BENE_ID, dgns_cd, washout_start_dt_possible_earliest, SRVC_BGN_DT)]
msk <- unique(msk, by = 1)

setnames(msk, "SRVC_BGN_DT", "msk_diagnosis_dt")

write_data(as.data.table(distinct(msk)), "m4806_washout_dts.fst", file.path(drv_root, "exclusion"))

# number of people with MSK pain claims
msk |> distinct() |> nrow() # 122,192
