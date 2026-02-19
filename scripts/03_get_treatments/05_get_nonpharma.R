# -------------------------------------
# Script: nonpharma
# Author: Anton Hung
# Updated:
# Purpose: get all dates for non-pharmacologic treatments, and record the type of treatment.
#           this is done by looping through the ICD-10 codes for the following types
#           of treatments:
#           1. Physical therapy
#           2. Chiropractic
#           3. Acupuncture
#           4. Blocks
#           5. Interventions (a group combining the following less common treatments:)
#             a. "Ablative techniques"
#             b. "Botulinum toxin injections"
#             c. "Electrical nerve stimulation"
#             d. "Intrathecal drug therapies"
#             e. "Epidural steroids"
#             f. "Minimally invasive spinal procedures"
#             g. "Trigger point injection"
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
cohort <- load_data("m4806_washout_dts.fst", file.path(drv_root, "exclusion")) |>
  as.data.table() |>
  mutate(exposure_end_dt_possible_latest = msk_diagnosis_dt + days(455))

# Read in CPT, HCPC, and Modifier codes for mediator claims
codes <- read_yaml("~/medicaid/M4806-diagnosis-cohort/data/public/mediator_codes.yml")
mediators <- c("Physical therapy",
               # "Counseling",
               # "Massage therapy",
               "Chiropractic",
               # "Acupuncture",
               "Blocks",
               "Ablative techniques", ##
               "Botulinum toxin injections", ## 
               "Electrical nerve stimulation", ##
               "Intrathecal drug therapies", ## 
               "Epidural steroids", ##
               "Minimally invasive spinal procedures", ##
               "Trigger point injection") ##
treatments_df <- data.frame()
for (mediator in mediators) {
  for (code in codes[[mediator]]){
    treatments_df <- rbind(treatments_df, 
                           data.frame(cd = names(code), 
                                      treatment = rep(mediator, length(names(code))))
    )
  }
}
treatments_df <- treatments_df  |>
  mutate(treatment = ifelse(treatment %in% c("Physical therapy",
                                             # "Acupuncture",
                                             "Chiropractic",
                                             "Blocks"), treatment, "Intervention"))

# Filter OTL to claims codes
claims_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |> 
  filter(LINE_PRCDR_CD %in% treatments_df$cd) |>
  collect()

setDT(claims)
setkey(claims, BENE_ID)

claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                     LINE_SRVC_END_DT, 
                                     LINE_SRVC_BGN_DT)]

# Inner join with cohort 
claims <- unique(merge(claims, cohort, by = "BENE_ID"))

# Filter to claims within mediator time-frame
claims <- claims[LINE_SRVC_BGN_DT %within% interval(msk_diagnosis_dt, 
                                                    exposure_end_dt_possible_latest), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, LINE_SRVC_END_DT, msk_diagnosis_dt, exposure_end_dt_possible_latest, LINE_PRCDR_CD)]

treatments_dts <- claims |>
  left_join(treatments_df |> distinct(cd, .keep_all=T), 
            by = c("LINE_PRCDR_CD" = "cd")) |>
  select(BENE_ID, treatment_start_dt = LINE_SRVC_BGN_DT, treatment_end_dt = LINE_SRVC_BGN_DT, treatment_name = treatment) |>
  arrange(treatment_start_dt, desc(treatment_end_dt))

write_data(unique(treatments_dts), "nonpharma_dts.fst", file.path(drv_root, "treatment"))
# saveRDS(treatments_dts, file.path(drv_root, "exclusion/treatments_dts.rds"))
