# -------------------------------------
# Script: nocens.R
# Author: Nick Williams
# Purpose:
# Notes:
# -------------------------------------

library(glue)
library(lmtp)
library(mlr3extralearners)

source("R/helpers.R")

data <- read_analytic("stenosis")

time_horizon <- 6

vars <- names(data)

trt <- glue("mo{seq_len(time_horizon)}_highrisk")
baseline <- c(
  vars[startsWith(vars, "dem")], 
  vars[startsWith(vars, "missing")], 
  vars[endsWith(vars, "washout_cal")]
)

time_vary <- lapply(seq_len(time_horizon), function(t) {
  glue("mo{t}_{c('days_supply', 'max_daily_dose_mme')}")
})

compete <- NULL
id <- NULL
cens <- glue("cens_period_{seq_len(time_horizon)}")
outcome <- c(glue("oud_period_{seq_len(time_horizon - 1)}"), "oud_period_6_12")

learners <- c("glm", "earth")

tmle <- lmtp_tmle(data, trt, outcome, baseline, cens = cens,
                  shift = NULL,
                  outcome_type = "survival",
                  learners_outcome = learners,
                  learners_trt = learners,
                  folds = 2,
                  control = lmtp_control(.learners_outcome_folds = 2, .learners_trt_folds = 2))

saveRDS(tmle, "/mnt/general-data/disability/M4806-diagnosis-cohort/history-mtp-application/tmle-nocens-fit.rds")

sdr <- lmtp_sdr(data, trt, outcome, baseline, cens = cens,
                shift = NULL,
                outcome_type = "survival",
                learners_outcome = learners,
                learners_trt = learners,
                folds = 2,
                control = lmtp_control(.learners_outcome_folds = 2, .learners_trt_folds = 2))

saveRDS(sdr, "/mnt/general-data/disability/M4806-diagnosis-cohort/history-mtp-application/sdr-nocens-fit.rds")

