
library(data.table)
library(tidyverse)
library(yaml)
library(glue)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

data <- load_data("msk_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  mutate(oud_period_12 = ifelse(cens_period_12 == 0, NA, oud_period_12))


table_one_function <- function(df){
  
  df <- df |>
    mutate(
      # dem_race_aian = ifelse(missing_dem_race==0, dem_race_aian, NA),
      dem_race_asian = ifelse(missing_dem_race==0, dem_race_asian, NA),
      dem_race_black = ifelse(missing_dem_race==0, dem_race_black, NA),
      # dem_race_hawaiian = ifelse(missing_dem_race==0, dem_race_hawaiian, NA),
      dem_race_hispanic = ifelse(missing_dem_race==0, dem_race_hispanic, NA),
      dem_race_other = ifelse(missing_dem_race==0, dem_race_other, NA),
      dem_race = ifelse(dem_race == "White, non-Hispanic", 1, 0),
      dem_race = ifelse(missing_dem_race == 0, dem_race, NA),
      dem_primary_language_english = ifelse(missing_dem_primary_language_english==0, dem_primary_language_english, NA),
      dem_married_or_partnered = ifelse(missing_dem_married_or_partnered==0, dem_married_or_partnered, NA),
      dem_household_size = ifelse(dem_household_size=="1", 1, 0),
      dem_household_size = ifelse(missing_dem_household_size==0, dem_household_size, NA),
      dem_household_size_2 = ifelse(missing_dem_household_size==0, dem_household_size_2, NA),
      dem_household_size_2plus = ifelse(missing_dem_household_size==0, dem_household_size_2plus, NA),
      dem_veteran = ifelse(missing_dem_veteran==0, dem_veteran, NA),
      dem_tanf_benefits = ifelse(missing_dem_tanf_benefits==0, dem_tanf_benefits, NA),
      dem_ssi_benefits_mandatory_optional = ifelse(missing_dem_ssi_benefits==0, dem_ssi_benefits_mandatory_optional, NA),
      dem_ssi_benefits = ifelse(dem_ssi_benefits=="Not Applicable", 1, 0),
      dem_ssi_benefits = ifelse(missing_dem_ssi_benefits==0, dem_ssi_benefits, NA),
      iph_1_washout_cal_bin = as.numeric(num_iph_washout_cal >= 1),
      oth_1_washout_cal_bin = as.numeric(num_oth_washout_cal > 0 & num_oth_washout_cal <= 25),
      oth_25_washout_cal_bin = as.numeric(num_oth_washout_cal > 25 & num_oth_washout_cal <= 50),
      oth_50_washout_cal_bin = as.numeric(num_oth_washout_cal > 50),
      ed_0_washout_cal_bin = as.numeric(n_ED_visits_washout_cal == 0),
      ed_1_washout_cal_bin = as.numeric(n_ED_visits_washout_cal %in% c(1,2)),
      ed_3_washout_cal_bin = as.numeric(n_ED_visits_washout_cal > 3)
    ) |>
    select(# baseline covariates
      dem_age, # CONTINUOUS
      dem_sex_m,
      starts_with("dem_race"),
      missing_dem_race,
      # race missing
      dem_primary_language_english,
      missing_dem_primary_language_english,
      dem_married_or_partnered,
      missing_dem_married_or_partnered,
      dem_household_size,
      dem_household_size_2,
      dem_household_size_2plus,
      missing_dem_household_size,
      dem_veteran,
      missing_dem_veteran,
      dem_probable_high_income,
      dem_tanf_benefits,
      missing_dem_tanf_benefits,
      dem_ssi_benefits_mandatory_optional,
      dem_ssi_benefits,
      missing_dem_ssi_benefits,
      # other SSI categories
      ends_with("cal"),
      -"num_iph_washout_cal", -"num_oth_washout_cal", -"n_ED_visits_washout_cal",
      ends_with("cal_bin"),
      # outcomes
      oud_period_12,
      cens_period_12
    )
  
  df <- df |>
    mutate(baseline_covariates = NA, .before = dem_age) |>
    mutate(sex = NA, .before = dem_sex_m) |>
    mutate(dem_sex_f = 1 - dem_sex_m, .after = dem_sex_m) |>
    mutate(race = NA, .before = dem_race) |>
    mutate(household_size = NA, .before = dem_household_size) |>
    mutate(ssi_benefits = NA, .before = dem_ssi_benefits_mandatory_optional) |>
    mutate(psychiatric_conditions = NA, .before = adhd_washout_cal) |>
    mutate(healthcare_utilization = NA, .before = iph_1_washout_cal_bin) |>
    # mutate(inpatient = NA, .before = iph_1_washout_cal_bin) |>
    mutate(outpatient = NA, .before = oth_1_washout_cal_bin) |>
    mutate(emergency = NA, .before = ed_0_washout_cal_bin) |>
    # mutate(diagnosis_codes = NA, .before = dgns_cd_M545) |>
    # mutate(treatments = NA, .before = exposure_acupuncture) |>
    mutate(outcomes = NA, .before = oud_period_12) |>
    # mutate(censoring = NA, .before = cens_period_12) |>
    # mutate(ed_visits = NA, .before = ed_visit_period_exposure) |>
    as.data.table()
  
  
  demographics <- c("\\textbf{Baseline covariates (washout period)}",
                    "Age",
                    "Sex",
                    "\\hspace{0.5cm}Male",
                    "\\hspace{0.5cm}Female",
                    "Race/ethnicity:",
                    "\\hspace{0.5cm}White, non-Hispanic",
                    # "\\hspace{0.5cm}AIAN, non-Hispanic",
                    "\\hspace{0.5cm}Asian, non-Hispanic",
                    "\\hspace{0.5cm}Black, non-Hispanic",
                    # "\\hspace{0.5cm}Hawaiian/Pacific Islander",
                    "\\hspace{0.5cm}Hispanic, all races",
                    # "\\hspace{0.5cm}Multiracial, non-Hispanic",
                    "\\hspace{0.5cm}Other races",
                    "\\hspace{0.5cm}Unknown",
                    "Primary language English",
                    "\\hspace{0.5cm}Unknown",
                    "Married/partnered",
                    "\\hspace{0.5cm}Unknown",
                    "Household size:",
                    "\\hspace{0.5cm}1",
                    "\\hspace{0.5cm}2",
                    "\\hspace{0.5cm}$\\geq$ 3",
                    "\\hspace{0.5cm}Unknown",
                    "Veteran",
                    "\\hspace{0.5cm}Unknown",
                    "High income ($>$ 138FPL)",
                    "TANF Benefits",
                    "\\hspace{0.5cm}Unknown",
                    "SSI benefits:",
                    "\\hspace{0.5cm}Mandatory or optional",
                    "\\hspace{0.5cm}Not applicable",
                    "\\hspace{0.5cm}Unknown",
                    "Psychiatric conditions \\& counseling:",
                    "\\hspace{0.5cm}ADHD",
                    "\\hspace{0.5cm}Anxiety",
                    "\\hspace{0.5cm}Bipolar",
                    "\\hspace{0.5cm}Depression",
                    "\\hspace{0.5cm}Other mental illness",
                    "\\hspace{0.5cm}Mental health counseling",
                    "Alcohol use disorder:",
                    "Other substance use disorder:",
                    "\\textbf{Healthcare Utilization}",
                    "Any inpatient hospitalization",
                    "Outpatient visits",
                    paste0("\\hspace{0.5cm}", c("1-25", "26-50", "$>$50")),
                    # "Prescriptions",
                    # paste0("\\hspace{0.5cm}", c("0", "1-10", "11-20", "21-30", "31+")),
                    "Emergency department",
                    paste0("\\hspace{0.5cm}", c("0", "1-2", "$>$3")),
                    "\\textbf{Outcomes (months 1-12)}",
                    "\\hspace{0.5cm}OUD by 12 months",
                    "\\hspace{0.5cm}Uncensored through 12 months"
  )
  
  ############# Preparing continuous variables
  continuous_vars <- c("dem_age"
                       # "num_iph_washout_cal",
                       # "num_oth_washout_cal",
                       # "num_rxl_washout_cal",
                       # "n_ED_visits_washout_cal",
                       # "exposure_max_daily_dose_mme",
                       # "exposure_days_supply"
                       # "ed_visit_period_exposure", 
                       # "ed_visit_period_1", 
                       # "ed_visit_period_2", 
                       # "ed_visit_period_3", 
                       # "ed_visit_period_4",
                       # "ed_visit_period_5"
  )
  continuous_names <- c("Age"
                        # "\\hspace{0.5cm}Inpatient",
                        # "\\hspace{0.5cm}Outpatient",
                        # "\\hspace{0.5cm}Prescriptions",
                        # "\\hspace{0.5cm}Emergency department",
                        # "Max MME",
                        # "Days supply"
                        # "\\hspace{0.5cm}Exposure",
                        # "\\hspace{0.5cm}0-3 months",
                        # "\\hspace{0.5cm}3-6 months",
                        # "\\hspace{0.5cm}6-9 months",
                        # "\\hspace{0.5cm}9-12 months",
                        # "\\hspace{0.5cm}12-15 months"
  )
  
  summarise_continuous_variable <- function(data, variable){
    data <- data[data[[variable]] > 0, ]
    return(paste0(round(median(data[[variable]], na.rm=T),2)," (",
                  round(quantile(data[[variable]], 0.25, na.rm=T),2),", ",
                  round(quantile(data[[variable]], 0.75, na.rm=T),2),")"))
  }
  
  continuous_values <- as.vector(sapply(continuous_vars, function(variable) summarise_continuous_variable(df, variable)))
  
  
  
  
  ############# Preparing binary variables
  number <- sapply(df, function(x) sum(x, na.rm=T))
  proportion <- sapply(df, function(x) mean(x, na.rm=T))
  number_proportion <- paste0(number, " (", round(proportion*100,2), "\\%)")
  
  
  
  ############# Putting everything together
  
  table_one <- data.table(
    Characteristic = demographics,
    `Number (\\%)` = number_proportion
  )
  
  table_one[Characteristic %in% continuous_names, "Number (\\%)"] <- continuous_values
  
  # table_one[["Number (\\%)"]] <- gsub("0 (NA\\%)", "", table_one[["Number (\\%)"]])
  
  # table_one_part1 <- table_one[1:38,]
  # table_one_part2 <- table_one[39:63,]
  table_one
  
}