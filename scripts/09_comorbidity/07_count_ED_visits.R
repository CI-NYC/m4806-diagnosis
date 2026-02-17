library(tidyverse)
library(data.table)
library(lubridate)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

cohort <- load_data("msk_cohort.fst", file.path(drv_root, "final"))

ED_visits <- load_data("ED_visits_cleaned_with_procedures_and_inpatients_excluded.fst", file.path(drv_root, "outcome"))

ED_visits <- ED_visits |>
  filter(BENE_ID %in% cohort$BENE_ID)

ED_visits <- cohort |>
    left_join(ED_visits) |>
    filter(ED_visit_dt >= washout_start_dt & ED_visit_dt <= washout_end_dt) |>
    group_by(BENE_ID) |>
    summarise(n_ED_visits_washout_cal = n()#,
                # first_ED_visit_dt = min(ED_visit_dt),
                # last_ED_visit_dt = max(ED_visit_dt)
                )
    
cohort <- cohort |>
    select(BENE_ID) |>
    left_join(ED_visits) |>
    mutate(n_ED_visits_washout_cal = replace_na(n_ED_visits_washout_cal, 0))

cohort <- cohort |>
  mutate(n_ED_visits_washout_cal = ifelse(n_ED_visits_washout_cal > 9, 9, n_ED_visits_washout_cal))

write_data(cohort, "cohort_num_ED_visits.fst", file.path(drv_root, "baseline"))
