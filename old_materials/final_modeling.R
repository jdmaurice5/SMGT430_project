library(tidyverse)
library(ranger)
library(vip)
set.seed(42)


data = read_csv("savant_data_with_clusters_and_preds.csv")

data <- data %>% 
  arrange(game_id, at_bat_number, pitch_number) %>%
  group_by(game_id, at_bat_number) %>%
  mutate(prev_cluster = lag(cluster)) %>%
  ungroup()

data %>% colnames()


data <- data %>%
  mutate(
    cluster = as.factor(cluster)
  )

data <- data %>%
  mutate(
    prev_cluster = ifelse(is.na(prev_cluster),
                             "None",
                             as.character(prev_cluster)),
    prev_cluster = as.factor(prev_cluster)
  )

data$swing

# ---- Previous pitch features ----
data <- data %>% 
  arrange(game_id, at_bat_number, pitch_number) %>%
  group_by(game_id, at_bat_number) %>%
  mutate(
    prev_swing = lag(swing),
    prev_zone = lag(zone)
  ) %>%
  ungroup()

# ---- Clean previous cluster ----
data <- data %>%
  mutate(
    prev_cluster = ifelse(is.na(prev_cluster), "None", as.character(prev_cluster)),
    prev_cluster = as.factor(prev_cluster),
    prev_swing = ifelse(is.na(prev_swing), 0, prev_swing),
    prev_zone = as.factor(prev_zone),
    zone=as.factor(zone),
    prev_zone = ifelse(is.na(prev_zone), 0, prev_zone)
  )





# ---- Convert cluster to factor ----
data <- data %>%
  mutate(
    cluster = as.factor(cluster)
  )



# ---- Filter player ----
diaz <- data %>% 
  filter(batter_name == "Díaz, Yandy")

# -----------------------------
# STAGE 2: Swing Quality Model
# -----------------------------

diaz_swings <- diaz %>%
  filter(swing == TRUE, !is.na(cluster), year==2025)

swing_quality_rf <- ranger(
  cluster ~ outs + inning + pitch_hand + bat_score_diff + count +
    prev_pitch_name + prev_cluster + prev_swing + pitch_name + zone,
  num.trees = 500,
  importance = "impurity",
  data = diaz_swings
)

diaz_swings=diaz_swings |> 
  mutate(pred_cluster = swing_quality_rf$predictions) 

swing_quality_rf_no_seq <- ranger(
  cluster ~ pitch_hand + pitch_name + zone,
  num.trees = 500,
  importance = "impurity",
  data = diaz_swings
)

diaz_swings=diaz_swings |> 
  mutate(pred_cluster_no_seq = swing_quality_rf_no_seq$predictions) 



diaz_swings %>% group_by(cluster) %>% summarize(n())
mean(diaz_swings$pred_cluster == diaz_swings$cluster, na.rm=TRUE)

test=diaz_swings %>% filter(pred_cluster==5, cluster == 5, pred_cluster_no_seq!=5, 
                            description %in%c("swinging_strike")) %>% 
                         select(game_date, inning, batter_name, count, pitch_name, prev_pitch_name, 
                                pred, cluster, pred_cluster, pred_cluster_no_seq)

test2=diaz_swings %>% filter(game_date == "2025-08-04", inning==7) %>% 
  select(game_date, inning, batter_name, count, pitch_name, prev_pitch_name, 
         pred, cluster, pred_cluster, pred_cluster_no_seq, zone, prev_zone, bat_speed, attack_angle)

set.seed(42)

train_idx <- sample(nrow(diaz_swings), 0.8 * nrow(diaz_swings))

train <- diaz_swings[train_idx, ]
test  <- diaz_swings[-train_idx, ]

pred_seq <- predict(swing_quality_rf, data = test)$predictions
pred_no_seq <- predict(swing_quality_rf_no_seq, data = test)$predictions


correct_seq <- pred_seq == test$cluster
correct_no_seq <- pred_no_seq == test$cluster

tab <- table(correct_seq, correct_no_seq)
tab

mcnemar.test(tab)

pred_seq <- diaz_swings$pred_cluster
pred_no_seq <- diaz_swings$pred_cluster_no_seq


correct_seq <- pred_seq==diaz_swings$cluster
correct_no_seq <- pred_no_seq == diaz_swings$cluster

mean(correct_no_seq)
mean(correct_seq)

tab <- table(correct_seq, correct_no_seq)
tab

mcnemar.test(tab)

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
diaz %>%
  filter(year==2025) %>%
  group_by(cluster) %>%
  filter(!is.na(cluster)) %>%
  summarise(
    `Pitches` = n(),
    `Squared Up %` = mean(squared_up == TRUE, na.rm = TRUE),
    `Contact %` = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    `xwOBA` = mean(expected_woba, na.rm = TRUE),
    `Swing Length` = mean(swing_length, na.rm = TRUE),
    `Bat Speed (mph)` = mean(bat_speed, na.rm = TRUE),
    `Attack Angle` = mean(attack_angle, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cluster = factor(cluster, levels = c(1, 2, 3, 4, 5))) %>%
  arrange(cluster) %>%
  gt(rowname_col = "cluster") %>%
  tab_header(
    title = md("**Yandy Díaz Swing Profile by Cluster**")
  ) %>%
  # label above 1,2,3
  tab_stubhead(label = "Cluster") %>%
  fmt_integer(
    columns = `Pitches`
  ) %>%
  fmt_percent(
    columns = c(`Squared Up %`, `Contact %`),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(`Swing Length`, `Attack Angle`, `Bat Speed (mph)`),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(`xwOBA`),
    decimals = 3
  ) %>%
  cols_align(
    align = "center",
    everything()
  )
library(gt)
library(scales)
diaz %>%
  filter(year==2025) %>%
  group_by(cluster) %>%
  filter(!is.na(cluster)) %>%
  summarise(
    `Pitches` = n(),
    `Squared Up %` = mean(squared_up == TRUE, na.rm = TRUE),
    `Contact %` = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    `xwOBA` = mean(expected_woba, na.rm = TRUE),
    `Swing Length` = mean(swing_length, na.rm = TRUE),
    `Bat Speed (mph)` = mean(bat_speed, na.rm = TRUE),
    `Attack Angle` = mean(attack_angle, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cluster = factor(cluster, levels = c(1, 2, 3, 4, 5))) %>%
  arrange(cluster) %>%
  gt(rowname_col = "cluster") %>%
  tab_header(
    title = md("**Yandy Díaz Swing Profile by Cluster**")
  ) %>%
  # label above 1,2,3
  tab_stubhead(label = "Cluster") %>%
  fmt_integer(
    columns = `Pitches`
  ) %>%
  fmt_percent(
    columns = c(`Squared Up %`, `Contact %`),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(`Swing Length`, `Attack Angle`, `Bat Speed (mph)`),
    decimals = 2
  ) %>%
  fmt_number(
    columns = c(`xwOBA`),
    decimals = 3
  ) %>%
  cols_align(
    align = "center",
    everything()
  )



