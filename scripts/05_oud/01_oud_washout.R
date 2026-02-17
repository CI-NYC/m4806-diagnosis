# -------------------------------------
# Script: 01_oud_washout.R
# Author: Nick Williams
# Updated:
# Purpose: Combine OUD expansive definitions to indicate OUD during washout period
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(collapse)
library(fst)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")
# save_dir <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort/exclusion"

cohort <- load_data("msk_washout_continuous_enrollment_dts.fst", file.path(drv_root, "exclusion"))

# load component files ----------------------------------------------------

poison <- load_data("oud_poison_dts.fst", file.path(drv_root, "exclusion"))
hillary <- load_data("oud_hillary_dts.fst", file.path(drv_root, "exclusion"))
misuse <- load_data("washout_oud_misuse.fst", file.path(drv_root, "exclusion"))
bup <- load_data("moud_bup_washout.fst", file.path(drv_root, "exclusion"))
methadone <- load_data("moud_methadone_washout.fst", file.path(drv_root, "exclusion"))
nal <- load_data("moud_nal_washout.fst", file.path(drv_root, "exclusion"))

# combine -----------------------------------------------------------------

# - combine MOUD files
cohort <- 
  list(bup, methadone, nal) |> 
  map(\(x) fselect(x, BENE_ID, 2)) |> 
  reduce(join, how = "left") |> 
  join(cohort, how = "left")

# - add probable misuse
cohort <- 
  fselect(misuse, BENE_ID, exclusion_oud_misuse) |> 
  join(cohort, how = "left")

# - add hillary codes
cohort <- 
  fmutate(hillary, 
          exclusion_oud_hillary = 
            as.numeric(oud_hillary_dt %within% interval(washout_start_dt, washout_end_dt))) |> 
  fselect(BENE_ID, exclusion_oud_hillary) |> 
  distinct() |> 
  join(cohort, how = "right") |> 
  fmutate(exclusion_oud_hillary = replace_na(exclusion_oud_hillary, value = 0))

# - add poison codes
cohort <- 
  fmutate(poison, 
          exclusion_oud_poison = 
            as.numeric(oud_poison_dt %within% interval(washout_start_dt, washout_end_dt))) |> 
  fselect(BENE_ID, exclusion_oud_poison) |> 
  distinct() |> 
  join(cohort, how = "right") |> 
  fmutate(exclusion_oud_poison = replace_na(exclusion_oud_poison, value = 0))

# - combine into one indicator
cohort <- 
  fmutate(cohort, 
          exclusion_oud = 
            as.numeric((exclusion_oud_poison + 
                          exclusion_oud_hillary + 
                          exclusion_oud_misuse + 
                          moud_bup_washout + 
                          moud_methadone_washout + 
                          moud_nal_washout) >= 1)) |> 
  fselect(BENE_ID, exclusion_oud)

# save --------------------------------------------------------------------

write_fst(
  cohort, 
  file.path(drv_root, "exclusion", 
            "oud_exclusion.fst")
)
