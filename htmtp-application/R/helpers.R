# -------------------------------------
# Script: R/helpers.R
# Author: Nick Williams
# Purpose:
# Notes:
# -------------------------------------

read_raw <- function(cohort) {
  if (cohort == "stenosis") {
    path <- "/mnt/general-data/disability/M4806-diagnosis-cohort/final/msk_cohort_clean_imputed.fst"
  } else {
    path <- "/mnt/general-data/disability/M4806-diagnosis-cohort/sciatica/final/cohort_clean_imputed.fst"
  }

  fst::read_fst(path)
}

read_analytic <- function(cohort) {
  if (cohort == "stenosis") {
    path <- "/mnt/general-data/disability/M4806-diagnosis-cohort/history-mtp-application/analytic.fst"
  } else {
    path <- "/mnt/general-data/disability/M4806-diagnosis-cohort/history-mtp-application/analytic-sciatica.fst"
  }
  fst::read_fst(path)
}
