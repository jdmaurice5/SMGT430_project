geom_histogram(bins = 30, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of Gain",
    x = "Gain",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
ggplot(all_results, aes(x = gain)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of Gain",
    x = "Gain",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
ggplot(all_results, aes(x = gain)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of Gain",
    x = "Gain",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
ggplot(all_results, aes(x = gain)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Sequencing Gain Distribution",
    x = "Gain",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
negatives = all_results %>% filter(gain<0)
positives = all_results %>% filter(gain>=0)
View(positives)
run_seq_test_v3 <- function(df) {
  tryCatch({
    df$cluster <- droplevels(df$cluster)
    if (nrow(df) < 100 || length(levels(df$cluster)) < 2) return(tibble())
    model_seq <- ranger(
      cluster ~ pitch_hand + count + outs +
        prev_pitch_name + prev_swing + prev_zone +
        pitch_name + zone,
      num.trees = 500,
      importance = "impurity",
      data = df,
      seed = 42
    )
    model_no_seq <- ranger(
      cluster ~ pitch_hand + pitch_name + zone,
      num.trees = 500,
      importance = "impurity",
      data = df,
      seed = 42
    )
    # Overall OOB accuracy
    acc_seq    <- mean(model_seq$predictions == df$cluster, na.rm = TRUE)
    acc_no_seq <- mean(model_no_seq$predictions == df$cluster, na.rm = TRUE)
    # Per-cluster: which swing type is most predictable with sequence info?
    cluster_gains <- map_dfr(levels(df$cluster), function(cl) {
      actual <- df$cluster == cl
      tibble(
        cluster       = cl,
        sens_seq      = mean(model_seq$predictions[actual] == cl, na.rm = TRUE),
        sens_no_seq   = mean(model_no_seq$predictions[actual] == cl, na.rm = TRUE),
        cluster_gain  = sens_seq - sens_no_seq,
        cluster_n     = sum(actual)
      )
    })
    list(
      overall = tibble(
        acc_seq,
        acc_no_seq,
        gain = acc_seq - acc_no_seq,
        n = nrow(df),
        n_clusters = length(levels(df$cluster))
      ),
      by_cluster = cluster_gains
    )
  }, error = function(e) NULL)
}
all_results <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  group_modify(~ {
    res <- run_seq_test_v3(.x)
    if (is.null(res)) return(tibble())
    res$overall
  }) %>%
  ungroup()
negatives = all_results %>% filter(gain<0)
positives = all_results %>% filter(gain>=0)
View(negatives)
negatives %>% summarize(mean(n_clusters))
positives %>% summarize(mean(n_clusters))
negatives %>% summarize(median(n_clusters))
positives %>% summarize(median(n_clusters))
run_seq_test_v3 <- function(df) {
  tryCatch({
    df$cluster <- droplevels(df$cluster)
    if (nrow(df) < 100 || length(levels(df$cluster)) < 2) return(tibble())
    model_seq <- ranger(
      cluster ~ pitch_hand + count + outs +
        prev_pitch_name + prev_swing + prev_zone +
        pitch_name + zone,
      num.trees = 500,
      importance = "impurity",
      data = df,
      seed = 42
    )
    model_no_seq <- ranger(
      cluster ~ pitch_hand + pitch_name + zone,
      num.trees = 500,
      importance = "impurity",
      data = df,
      seed = 42
    )
    # Overall OOB accuracy
    acc_seq    <- mean(model_seq$predictions == df$cluster, na.rm = TRUE)
    acc_no_seq <- mean(model_no_seq$predictions == df$cluster, na.rm = TRUE)
    # Per-cluster: which swing type is most predictable with sequence info?
    cluster_gains <- map_dfr(levels(df$cluster), function(cl) {
      actual <- df$cluster == cl
      tibble(
        cluster       = cl,
        sens_seq      = mean(model_seq$predictions[actual] == cl, na.rm = TRUE),
        sens_no_seq   = mean(model_no_seq$predictions[actual] == cl, na.rm = TRUE),
        cluster_gain  = sens_seq - sens_no_seq,
        cluster_n     = sum(actual)
      )
    })
    imp <- model_seq$variable.importance
    imp_tbl <- tibble(
      variable = names(imp),
      importance = as.numeric(imp)
    ) %>%
      arrange(desc(importance)) %>%
      mutate(rank = row_number())
    seq_vars <- c("prev_pitch_name", "prev_swing", "prev_zone")
    seq_importance <- imp_tbl %>%
      filter(variable %in% seq_vars)
    list(
      overall = tibble(
        acc_seq,
        acc_no_seq,
        gain = acc_seq - acc_no_seq,
        n = nrow(df),
        n_clusters = length(levels(df$cluster))
      ),
      by_cluster = cluster_gains,
      importance = seq_importance
    )
  }, error = function(e) NULL)
}
importance_results <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  group_modify(~ {
    res <- run_seq_test_v3(.x)
    if (is.null(res)) return(tibble())
    res$importance
  }) %>%
  ungroup()
importance_results %>%
  group_by(variable) %>%
  summarise(
    avg_rank = mean(rank, na.rm = TRUE),
    avg_importance = mean(importance, na.rm = TRUE)
  )
merged <- all_results %>%
  left_join(
    importance_results %>%
      group_by(batter_name, year) %>%
      summarise(avg_seq_importance = mean(importance)),
    by = c("batter_name", "year")
  )
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average Sequencing Variable Importance",
    y = "Sequencing Gain"
  )
lm_fit <- lm(gain ~ avg_seq_importance, data = merged)
summary(lm_fit)
correlation <- cor(merged$avg_seq_importance,
                   merged$gain,
                   use = "complete.obs")
correlation
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average Sequencing Variable Importance",
    y = "Sequencing Gain"
  )
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average Sequencing Variable Importance",
    y = "Sequencing Gain"
  )+
  theme_minimal(base_size = 20)
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average Sequencing Variable Importance",
    y = "Sequencing Gain"
  )+
  theme_minimal(base_size = 20)
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average Sequencing Variable Importance",
    y = "Sequencing Gain"
  )+
  theme_minimal(base_size = 18)
ggplot(all_results, aes(x = gain)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Sequencing Gain Distribution",
    x = "Gain",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
View(all_results)
all_results %>% summarize(mean(acc_seq), mean(acc_no_seq))
importance_results %>%
  group_by(variable) %>%
  summarise(
    avg_rank = mean(rank, na.rm = TRUE),
    avg_importance = mean(importance, na.rm = TRUE)
  )
ggplot(all_results, aes(x = factor(n_clusters), y = gain)) +
  geom_boxplot() +
  labs(x = "Number of Swing Clusters", y = "Sequencing Gain")
ggplot(all_results, aes(x = gain)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Sequencing Gain Distribution",
    x = "Gain",
    y = "Count"
  ) +
  theme_minimal(base_size = 18)
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Sequencing Importance vs Predictive Gain",
    subtitle = "Small but significant relationship (R² = 0.078)",
    x = "Average Sequencing Importance",
    y = "Sequencing Gain"
  ) +
  theme_minimal(base_size = 14)
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sequencing Importance vs Predictive Gain",
    subtitle = "Small but significant relationship (R² = 0.078)",
    x = "Average Sequencing Importance",
    y = "Sequencing Gain"
  ) +
  theme_minimal(base_size = 14)
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sequencing Importance vs Predictive Gain",
    subtitle = "Small but significant relationship (R² = 0.078)",
    x = "Average Sequencing Importance",
    y = "Sequencing Gain"
  ) +
  theme_minimal(base_size = 18)
# ---- Filter player ----
diaz <- data %>%
  filter(batter_name == "Díaz, Yandy")
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
diaz_swings %>% group_by(cluster) %>% summarize(n())
mean(diaz_swings$pred_cluster == diaz_swings$cluster, na.rm=TRUE)
test=diaz_swings %>% filter(pred_cluster==5, cluster == 5, pred_cluster_no_seq!=5,
                            description %in%c("swinging_strike")) %>%
  select(game_date, inning, batter_name, count, pitch_name, prev_pitch_name,
         pred, cluster, pred_cluster, pred_cluster_no_seq)
swing_quality_rf_no_sequencing <- ranger(
  cluster ~ outs + inning + pitch_hand + bat_score_diff + count +
    prev_cluster + prev_swing + pitch_name + zone,
  num.trees = 500,
  importance = "impurity",
  data = diaz_swings
)
diaz_swings=diaz_swings |>
  mutate(pred_cluster_no_seq = swing_quality_rf_no_sequencing$predictions)
test=diaz_swings %>% filter(pred_cluster==5, cluster == 5, pred_cluster_no_seq!=5,
                            description %in%c("swinging_strike")) %>%
  select(game_date, inning, batter_name, count, pitch_name, prev_pitch_name,
         pred, cluster, pred_cluster, pred_cluster_no_seq)
View(test)
test2=diaz_swings %>% filter(game_date == "2025-08-04", inning==7) %>%
  select(game_date, inning, batter_name, count, pitch_name, prev_pitch_name,
         pred, cluster, pred_cluster, pred_cluster_no_seq, zone, prev_zone, bat_speed,
         swing_length, intercept_ball_minus_batter_pos_y_inches)
View(test2)
test3=data %>% filter(game_date == "2025-08-04", inning==7, batter_name == "Díaz, Yandy") %>%
  select(game_date, inning, batter_name, count, pitch_name, prev_pitch_name,
         pred, cluster, zone, prev_zone, bat_speed,
         swing_length, intercept_ball_minus_batter_pos_y_inches)
View(test3)
test2=diaz_swings %>% filter(game_date == "2025-08-04", inning==7) %>%
  select(game_date, inning, batter_name, count, pitch_name, prev_pitch_name,
         pred, cluster, pred_cluster, pred_cluster_no_seq, zone, prev_zone, bat_speed,
         swing_length, intercept_ball_minus_batter_pos_y_inches, swing_path_tilt)
diaz %>% filter(year==2025) %>% group_by(cluster) %>% filter(!is.na(cluster)) %>%
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
diaz %>% filter(year==2025) %>% group_by(cluster) %>% filter(!is.na(cluster)) %>%
  summarise(
    n = n(),
    squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
    contact_pct = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    xwoba = mean(expected_woba, na.rm = TRUE),
    swing_length = mean(swing_length, na.rm=TRUE),
    intercept=mean(intercept_ball_minus_batter_pos_y_inches, na.rm=TRUE),
    bat_speed=mean(bat_speed, na.rm=TRUE),
    swing_path_tilt=mean(swing_path_tilt, na.rm=TRUE),
    .groups = "drop"
  )
diaz %>% filter(year==2025) %>% group_by(cluster) %>% filter(!is.na(cluster)) %>%
  summarise(
    n = n(),
    squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
    contact_pct = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
    xwoba = mean(expected_woba, na.rm = TRUE),
    swing_length = mean(swing_length, na.rm=TRUE),
    intercept=mean(intercept_ball_minus_batter_pos_y_inches, na.rm=TRUE),
    bat_speed=mean(bat_speed, na.rm=TRUE),
    swing_path_tilt=mean(attack_angle, na.rm=TRUE),
    .groups = "drop"
  )
test2=diaz_swings %>% filter(game_date == "2025-08-04", inning==7) %>%
  select(game_date, inning, batter_name, count, pitch_name, prev_pitch_name,
         pred, cluster, pred_cluster, pred_cluster_no_seq, zone, prev_zone, bat_speed,
         swing_length, intercept_ball_minus_batter_pos_y_inches, attack_angle)
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
lm_fit <- lm(gain ~ avg_seq_importance, data = merged)
summary(lm_fit)
View(importance_results)
View(merged)
merged2 <- all_results %>%
  left_join(
    importance_results,
    by = c("batter_name", "year")
  )
View(merged2)
df=merged2
df %>%
  mutate(gain_direction = ifelse(gain > 0, "Positive gain", "Negative gain")) %>%
  group_by(variable, gain_direction) %>%
  summarise(avg_rank = mean(rank), .groups = "drop") %>%
  ggplot(aes(x = variable, y = avg_rank, fill = gain_direction)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Positive gain" = "#E24B4A", "Negative gain" = "#378ADD")) +
  labs(
    x = "Variable",
    y = "Average rank",
    fill = NULL
  ) +
  theme_minimal()
summary(lm_fit)
ggplot(all_results, aes(x = factor(n_clusters), y = gain)) +
  geom_boxplot() +
  labs(x = "Number of Swing Clusters", y = "Sequencing Gain")
ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Average Sequencing Variable Importance",
    y = "Sequencing Gain"
  )+
  theme_minimal(base_size = 18)
lm_fit <- lm(gain ~ avg_seq_importance, data = merged)
summary(lm_fit)
correlation <- cor(merged$avg_seq_importance,
                   merged$gain,
                   use = "complete.obs")
correlation
summary(lm_fit)