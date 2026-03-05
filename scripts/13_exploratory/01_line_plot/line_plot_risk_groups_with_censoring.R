library(tidyverse)
library(fst)
library(lubridate)
library(collapse)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

data <- load_data("msk_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  select(BENE_ID, ends_with("highrisk"), starts_with("cens"), ends_with("treated"))

# rename cens columns
old <- grep("^cens_period_", names(data), value = TRUE)
new <- sub("^cens_period_(\\d+)$", "mo\\1_cens", old)
setnames(data, old, new)

data <- data |>
  pivot_longer(
    cols      = -BENE_ID,
    names_to = c("month", ".value"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) |>
  # remove mo and _highrisk from month variable and convert to numeric
  mutate(month = str_remove(month, "mo"),
         month = str_remove(month, "_highrisk"), 
         month = as.numeric(month),
         highrisk = ifelse(cens==0, NA, highrisk),
         treated = ifelse(cens==0, NA, treated)) |>
  arrange(BENE_ID, month)

# 1. Proportion of people with high risk treatment, among uncensored
df <- data |>
  group_by(month) |>
  summarise(percent = mean(highrisk, na.rm=TRUE))

p1 <- ggplot(df, aes(x=month, y=percent)) +
  geom_area(fill = "skyblue", alpha = 0.3) +
  geom_point(size=4) +
  geom_text(aes(label = round(percent, 3)*100, vjust = -0.8)) +
  labs(x="Month",
       y="Patients (%)") +
  scale_x_discrete(limits=as.character(1:12)) +
  theme_light()

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/highrisk_treatment_percentage_withcens.pdf", p1, width=10, height=5.5)


# 2. Proportion of people with high risk treatment, among treated
df <- data |>
  mutate(highrisk = ifelse(treated == 0, NA, highrisk)) |>
  group_by(month) |>
  summarise(percent = mean(highrisk, na.rm=TRUE))

p1 <- ggplot(df, aes(x=month, y=percent)) +
  geom_area(fill = "skyblue", alpha = 0.3) +
  geom_point(size=4) +
  geom_text(aes(label = round(percent, 3)*100, vjust = -0.8)) +
  labs(x="Month",
       y="Patients (%)") +
  scale_x_discrete(limits=as.character(1:12)) +
  theme_light()

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/highrisk_treatment_percentage_among_treated.pdf", p1, width=10, height=5.5)


# 3. calculating change at every month
df <- data |>
  group_by(BENE_ID) |>
  mutate(change = case_when(
    cens==0 ~ "Censored",
    treated==0 ~ "Untreated",
    lag(highrisk) == 0 & highrisk == 1 ~ "Risk increase",  # became high risk
    lag(highrisk) == 1 & highrisk == 0 ~ "Risk decrease",  # became low risk
    lag(highrisk) == 1 & highrisk == 1 ~ "Remained high risk",  # stayed high
    lag(highrisk) == 0 & highrisk == 0 ~ "Remained low risk",  # stayed low
  )) |>
  filter(!month==1)


df <- df |>
  # filter(!change == "Censored") |>
  group_by(month, change) |>
  summarise(percent = n(), .groups = "drop_last") |>
  mutate(prop = percent / sum(percent) * 100) |>
  ungroup() |>
  mutate(change = factor(change, levels = c("Risk increase", "Remained high risk", "Remained low risk", "Risk decrease", "Untreated", "Censored")))

df_rib <- df |>
  group_by(month) |>
  arrange(change) |>
  mutate(
    ymin = lag(cumsum(prop), default = 0),
    ymax = cumsum(prop)
  ) |>
  filter(!month == 1)


p3 <- ggplot(df_rib) +
  geom_ribbon(aes(x = month, ymin = ymin, ymax = ymax, fill = change), alpha = 0.6) +
  geom_text(
    data = df |>
      group_by(month) |>
      arrange(change) |>
      mutate(
        cum = cumsum(prop),
        mid = cum - prop/2        # vertical midpoint of each band
      ),
    aes(x = month, y = mid, label = round(prop,1)),
    color = "black",
    size = 3
  ) +
  scale_fill_manual(values = c("red", "pink", "lightblue", "blue", "grey","grey25")) +
  labs(x = "Month",
       y = "Group percentages") +
  scale_x_discrete(limits = as.character(1:12)) +
  theme_light()

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/change_in_risk.pdf", p3, width=10, height=5.5)
