library(tidyverse)
library(fst)
library(lubridate)
library(collapse)

source("~/medicaid/M4806-diagnosis-cohort/R/helpers.R")

data <- load_data("msk_cohort_clean_imputed.fst", file.path(drv_root, "final")) |>
  select(BENE_ID, ends_with("highrisk"))

df <- data |>
  pivot_longer(
    cols      = -BENE_ID,
    names_to  = "month",
    values_to = "highrisk"
  ) |>
  # remove mo and _highrisk from month variable and convert to numeric
  mutate(month = str_remove(month, "mo")) |>
  mutate(month = str_remove(month, "_highrisk")) |>
  mutate(month = as.numeric(month))

df <- df |>
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

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/highrisk_treatment_percentage.pdf", p1, width=10, height=5.5)

# new plot, with those who are already high risk in the first month excluded
df <- data |>
  filter(mo1_highrisk == 0) |>
  pivot_longer(
    cols      = -BENE_ID,
    names_to  = "month",
    values_to = "highrisk"
  ) |>
  # remove mo and _highrisk from month variable and convert to numeric
  mutate(month = str_remove(month, "mo")) |>
  mutate(month = str_remove(month, "_highrisk")) |>
  mutate(month = as.numeric(month))

df <- df |>
  group_by(month) |>
  summarise(percent = mean(highrisk, na.rm=TRUE))

p2 <- ggplot(df, aes(x=month, y=percent)) +
  geom_area(fill = "skyblue", alpha = 0.3) +
  geom_point(size=4) +
  geom_text(aes(label = round(percent, 3)*100, vjust = -0.8)) +
  labs(x="Month",
       y="Patients (%)") +
  scale_x_discrete(limits=as.character(1:12)) +
  theme_light()

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/among_lowrisk_treatment_percentage.pdf", p2, width=10, height=5.5)


# 3. calculating change at every month
df <- data |>
  pivot_longer(
    cols      = -BENE_ID,
    names_to  = "month",
    values_to = "highrisk"
  ) |>
  # remove mo and _highrisk from month variable and convert to numeric
  mutate(month = str_remove(month, "mo")) |>
  mutate(month = str_remove(month, "_highrisk")) |>
  mutate(month = as.numeric(month)) |>
  group_by(BENE_ID) |>
  # arrange(month) |>
  mutate(change = case_when(
    lag(highrisk) == 0 & highrisk == 1 ~ "Risk increase",  # became high risk
    lag(highrisk) == 1 & highrisk == 0 ~ "Risk decrease",  # became low risk
    lag(highrisk) == 1 & highrisk == 1 ~ "Remained high risk",  # stayed high
    lag(highrisk) == 0 & highrisk == 0 ~ "Remained low risk",  # stayed low
    )) |>
  filter(!is.na(change))


df <- df |>
  group_by(month, change) |>
  summarise(percent = n(), .groups = "drop_last") |>
  mutate(prop = percent / sum(percent) * 100) |>
  ungroup() |>
  mutate(change = factor(change, levels = c("Risk increase", "Remained high risk", "Remained low risk", "Risk decrease")))

df_rib <- df |>
  group_by(month) |>
  arrange(change) |>
  mutate(
    ymin = lag(cumsum(prop), default = 0),
    ymax = cumsum(prop)
  )

# # make area plot of the change in high risk patients over time
# p3 <- ggplot(df, aes(x = month, y = prop, fill = change)) +
#   geom_ribbon(aes(x = month), position = "stack", alpha = 0.6) +
#   # add labels inside each stacked area
#   geom_text(
#     data = df |>
#       group_by(month) |>
#       arrange(change) |>
#       mutate(
#         cum = cumsum(prop),
#         mid = cum - prop/2        # vertical midpoint of each band
#       ),
#     aes(x = month, y = mid, label = round(prop,1)),
#     color = "black",
#     size = 3
#   ) +
#   scale_fill_manual(values = c("red", "white", "blue")) +
#   labs(x = "Month",
#        y = "Group percentages") +
#   scale_x_discrete(limits = as.character(1:12)) +
#   theme_light()



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
  scale_fill_manual(values = c("blue", "lightblue", "pink", "red")) +
    labs(x = "Month",
         y = "Group percentages") +
    scale_x_discrete(limits = as.character(1:12)) +
  theme_light()

ggsave("~/medicaid/M4806-diagnosis-cohort/scripts/13_exploratory/01_line_plot/change_in_risk.pdf", p3, width=10, height=5.5)
