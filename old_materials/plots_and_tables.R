library(tidyverse)
library(ranger)
library(vip)
library(gt)
library(scales)
set.seed(42)


data = read_csv("savant_data_with_clusters_and_preds.csv")


witt = data %>% filter(batter_name=="Witt Jr., Bobby", year == 2025)

diaz %>% filter(year==2025) %>% group_by(cluster) %>% filter(!is.na(cluster)) %>% 
  summarise(
    n = n(),
    squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
    contact_pct = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    xwoba = mean(expected_woba, na.rm = TRUE),
    swing_length = mean(swing_length, na.rm=TRUE),
    intercept=mean(intercept_ball_minus_batter_pos_y_inches, na.rm=TRUE),
    bat_speed=mean(bat_speed, na.rm=TRUE),
    attack_angle=mean(attack_angle, na.rm=TRUE),
    .groups = "drop"
  )

judge = data %>% filter(batter_name=="Judge, Aaron", year == 2025)

judge %>% group_by(cluster) %>% filter(!is.na(cluster)) %>% 
  summarise(
    n = n(),
    squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
    contact_pct = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    xwoba = mean(expected_woba, na.rm = TRUE),
    swing_length = mean(swing_length, na.rm=TRUE),
    intercept=mean(intercept_ball_minus_batter_pos_y_inches, na.rm=TRUE),
    bat_speed=mean(bat_speed, na.rm=TRUE),
    .groups = "drop"
  )



library(dplyr)
library(ggplot2)

library(ggplot2)

ggplot(all_results, aes(x = gain)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of Gain",
    x = "Gain",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)



library(dplyr)
library(gt)

diaz %>%
# Create formatted table for Yandy Díaz
diaz_table <- diaz %>%
  group_by(cluster) %>%
  filter(!is.na(cluster)) %>%
  summarise(
    n = n(),
    squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
    contact_pct = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    xwoba = mean(expected_woba, na.rm = TRUE),
    swing_length = mean(swing_length, na.rm = TRUE),
    intercept = mean(intercept_ball_minus_batter_pos_y_inches, na.rm = TRUE),
    bat_speed = mean(bat_speed, na.rm = TRUE),
    attack_angle = mean(attack_angle, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    `Cluster` = cluster,
    `N` = n,
    `Squared Up %` = squared_up_pct,
    `Contact %` = contact_pct,
    `xWOBA` = xwoba,
    `Swing Length` = swing_length,
    `Intercept` = intercept,
    `Bat Speed` = bat_speed,
    `Attack Angle` = attack_angle
  ) %>%
  gt() %>%
  tab_header(
    title = "Yandy Díaz - Swing Quality by Cluster",
    subtitle = "2025 Season"
  ) %>%
  fmt_percent(columns = c(`Squared Up %`, `Contact %`), decimals = 1) %>%
  fmt_number(columns = -`Cluster`, decimals = 2) %>%
  cols_align(align = "center", -`Cluster`)

# Create formatted table for Aaron Judge
judge_table <- judge %>%
  group_by(cluster) %>%
  filter(!is.na(cluster)) %>%
  summarise(
    n = n(),
    squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
    contact_pct = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    xwoba = mean(expected_woba, na.rm = TRUE),
    swing_length = mean(swing_length, na.rm = TRUE),
    intercept = mean(intercept_ball_minus_batter_pos_y_inches, na.rm = TRUE),
    bat_speed = mean(bat_speed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    `Cluster` = cluster,
    `N` = n,
    `Squared Up %` = squared_up_pct,
    `Contact %` = contact_pct,
    `xWOBA` = xwoba,
    `Swing Length` = swing_length,
    `Intercept` = intercept,
    `Bat Speed` = bat_speed
  ) %>%
  gt() %>%
  tab_header(
    title = "Aaron Judge - Swing Quality by Cluster",
    subtitle = "2025 Season"
  ) %>%
  fmt_percent(columns = c(`Squared Up %`, `Contact %`), decimals = 1) %>%
  fmt_number(columns = -`Cluster`, decimals = 2) %>%
  cols_align(align = "center", -`Cluster`)

# Create formatted table for Bobby Witt Jr.
witt_table <- witt %>%
  group_by(cluster) %>%
  filter(!is.na(cluster)) %>%
  summarise(
    n = n(),
    squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
    contact_pct = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    xwoba = mean(expected_woba, na.rm = TRUE),
    swing_length = mean(swing_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    `Cluster` = cluster,
    `N` = n,
    `Squared Up %` = squared_up_pct,
    `Contact %` = contact_pct,
    `xWOBA` = xwoba,
    `Swing Length` = swing_length
  ) %>%
  gt() %>%
  tab_header(
    title = "Bobby Witt Jr. - Swing Quality by Cluster",
    subtitle = "2025 Season"
  ) %>%
  fmt_percent(columns = c(`Squared Up %`, `Contact %`), decimals = 1) %>%
  fmt_number(columns = -`Cluster`, decimals = 2) %>%
  cols_align(align = "center", -`Cluster`)

# Print tables
print(diaz_table)
print(judge_table)
print(witt_table)

# Additional visualizations
# Bat speed vs swing length by cluster
ggplot(data %>% filter(batter_name %in% c("Judge, Aaron", "Ohtani, Shohei", "Díaz, Yandy"), year == 2025), 
       aes(x = bat_speed, y = swing_length, color = cluster)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~batter_name) +
  labs(title = "Bat Speed vs Swing Length by Cluster",
       x = "Bat Speed (mph)",
       y = "Swing Length (ft)") +
  theme_minimal()

# Attack angle distribution by cluster
ggplot(data %>% filter(year == 2025, !is.na(cluster)), 
       aes(x = attack_angle, fill = cluster)) +
  geom_density(alpha = 0.4) +
  labs(title = "Attack Angle Distribution by Cluster",
       x = "Attack Angle (°)",
       y = "Density") +
  theme_minimal()

# Contact point by cluster
ggplot(data %>% filter(year == 2025, !is.na(cluster)), 
       aes(x = cluster, y = intercept_ball_minus_batter_pos_y_inches, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Contact Point by Cluster",
       x = "Cluster",
       y = "Intercept (inches)") +
  theme_minimal()
