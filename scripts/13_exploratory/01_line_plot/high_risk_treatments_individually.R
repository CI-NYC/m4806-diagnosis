library(tidyverse)
library(fst)
library(lubridate)
library(collapse)
library(data.table)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

data <- load_data("msk_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  select(BENE_ID, starts_with("cens")) |>
  left_join(load_data("followup_treatments_specific.fst", file.path(drv_root, "outcome"))) |>
  mutate(across(everything(), ~replace_na(.x, 0)))

# rename opioid columns
opioid_cols <- c(
  paste0("mo", seq_len(num_periods), "_Opioid_g50mme"),
  paste0("mo", seq_len(num_periods), "_Opioid_g7days_le50mme")
)

prefix <- sub("^([^_]+_).*", "\\1", opioid_cols)
rest   <- sub("^[^_]+_(.*)$", "\\1", opioid_cols)

new_names <- paste0(prefix, gsub("_", "", rest))

setnames(data, old = opioid_cols, new = new_names)

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
         month = as.numeric(month),
         across(!all_of(c("BENE_ID", "month", "cens")), ~ifelse(cens==0, NA, .x))) |>
  arrange(BENE_ID, month)


# 1. Proportion of people with high risk treatment, among uncensored
df <- data |>
  group_by(month) |>
  summarise(Gabapentin = mean(Gabapentin, na.rm=TRUE),
            Benzodiazepine = mean(Benzodiazepine, na.rm=TRUE),
            `Opioid>50mme` = mean(Opioidg50mme, na.rm=TRUE),
            `Opioid>7days<=50mme` = mean(Opioidg7daysle50mme, na.rm=TRUE),
            ) |>
  pivot_longer(cols = c("Gabapentin","Benzodiazepine","Opioid>50mme","Opioid>7days<=50mme"),
               names_to = "treatment",
               values_to = "percent")

p1 <- ggplot(df, aes(x=month, y=percent)) +
  geom_area(fill = "skyblue", alpha = 0.3) +
  geom_point(size=4) +
  geom_text(aes(label = round(percent, 3)*100, vjust = -0.8)) +
  labs(x="Month",
       y="Patients (%)") +
  facet_wrap(~ treatment) +
  scale_x_discrete(limits=as.character(1:12)) +
  theme_light() +
  theme(strip.text = element_text(color = "black"))

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/treatment_percentages_faceted_withcens.pdf", p1, width=15, height=9)


# 2. Proportion of people with high risk treatment, among treated
df <- data |>
  mutate(across(!all_of(c("BENE_ID", "month", "cens")), ~ifelse(treated==0, NA, .x))) |>
  group_by(month) |>
  summarise(Gabapentin = mean(Gabapentin, na.rm=TRUE),
            Benzodiazepine = mean(Benzodiazepine, na.rm=TRUE),
            `Opioid>50mme` = mean(Opioidg50mme, na.rm=TRUE),
            `Opioid>7days<=50mme` = mean(Opioidg7daysle50mme, na.rm=TRUE),
  ) |>
  pivot_longer(cols = c("Gabapentin","Benzodiazepine","Opioid>50mme","Opioid>7days<=50mme"),
               names_to = "treatment",
               values_to = "percent")

p2 <- ggplot(df, aes(x=month, y=percent)) +
  geom_area(fill = "skyblue", alpha = 0.3) +
  geom_point(size=4) +
  geom_text(aes(label = round(percent, 3)*100, vjust = -0.8)) +
  labs(x="Month",
       y="Patients (%)") +
  facet_wrap(~ treatment) +
  scale_x_discrete(limits=as.character(1:12)) +
  theme_light() +
  theme(strip.text = element_text(color = "black"))

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/treatment_percentages_faceted_amongtreated.pdf", p2, width=10, height=5.5)



# 3a. calculating change at every month (gabapentin)
df <- data |>
  group_by(BENE_ID) |>
  mutate(change = case_when(
    cens==0 ~ "Censored",
    treated==0 ~ "Untreated",
    lag(Gabapentin) == 0 & Gabapentin == 1 ~ "+ gabapentin",  # became high risk
    lag(Gabapentin) == 1 & Gabapentin == 0 ~ "- gabapentin",  # became low risk
    lag(Gabapentin) == 1 & Gabapentin == 1 ~ "still on gabapentin",  # stayed high
    lag(Gabapentin) == 0 & Gabapentin == 0 ~ "still off gabapentin",  # stayed low
  )) |>
  filter(!month==1)


df <- df |>
  group_by(month, change) |>
  summarise(percent = n(), .groups = "drop_last") |>
  mutate(prop = percent / sum(percent) * 100) |>
  ungroup() |>
  mutate(change = factor(change, levels = c("+ gabapentin", "still on gabapentin", "still off gabapentin", "- gabapentin", "Untreated", "Censored")))

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

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/change_in_gabapentin.pdf", p3, width=10, height=5.5)




# 3c. calculating change at every month (opioid > 50 mme)
df <- data |>
  group_by(BENE_ID) |>
  mutate(change = case_when(
    cens==0 ~ "Censored",
    treated==0 ~ "Untreated",
    lag(Opioidg50mme) == 0 & Opioidg50mme == 1 ~ "+ Opioid>50mme",  # became high risk
    lag(Opioidg50mme) == 1 & Opioidg50mme == 0 ~ "- Opioid>50mme",  # became low risk
    lag(Opioidg50mme) == 1 & Opioidg50mme == 1 ~ "still on Opioid>50mme",  # stayed high
    lag(Opioidg50mme) == 0 & Opioidg50mme == 0 ~ "still off Opioid>50mme",  # stayed low
  )) |>
  filter(!month==1)


df <- df |>
  group_by(month, change) |>
  summarise(percent = n(), .groups = "drop_last") |>
  mutate(prop = percent / sum(percent) * 100) |>
  ungroup() |>
  mutate(change = factor(change, levels = c("+ Opioid>50mme", "still on Opioid>50mme", "still off Opioid>50mme", "- Opioid>50mme", "Untreated", "Censored")))

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

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/change_in_Opioid>50mme.pdf", p3, width=10, height=5.5)



# 3d. calculating change at every month (opioid > 7 days)
df <- data |>
  group_by(BENE_ID) |>
  mutate(change = case_when(
    cens==0 ~ "Censored",
    treated==0 ~ "Untreated",
    lag(Opioidg7daysle50mme) == 0 & Opioidg7daysle50mme == 1 ~ "+ Opioid>7days<=50mme",  # became high risk
    lag(Opioidg7daysle50mme) == 1 & Opioidg7daysle50mme == 0 ~ "- Opioid>7days<=50mme",  # became low risk
    lag(Opioidg7daysle50mme) == 1 & Opioidg7daysle50mme == 1 ~ "still on Opioid>7days<=50mme",  # stayed high
    lag(Opioidg7daysle50mme) == 0 & Opioidg7daysle50mme == 0 ~ "still off Opioid>7days<=50mme",  # stayed low
  )) |>
  filter(!month==1)


df <- df |>
  group_by(month, change) |>
  summarise(percent = n(), .groups = "drop_last") |>
  mutate(prop = percent / sum(percent) * 100) |>
  ungroup() |>
  mutate(change = factor(change, levels = c("+ Opioid>7days<=50mme", "still on Opioid>7days<=50mme", "still off Opioid>7days<=50mme", "- Opioid>7days<=50mme", "Untreated", "Censored")))

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

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/change_in_Opioid>7days<=50mme.pdf", p3, width=10, height=5.5)