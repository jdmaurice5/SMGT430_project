library(tidyverse)
library(ranger)

set.seed(42)

data = read_csv("data/savant_data_with_clusters_and_preds.csv")

data <- data %>% 
  arrange(game_id, at_bat_number, pitch_number) %>%
  group_by(game_id, at_bat_number) %>%
  mutate(prev_cluster = lag(cluster)) %>%
  ungroup()


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


# ---- Filter to hitters with enough swings ----

min_swings <- 500

hitter_counts <- data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  summarize(n_swings = n(), .groups = "drop") %>%
  filter(n_swings >= min_swings)

filtered_data <- data %>%
  select(-any_of("n_swings")) %>%
  inner_join(hitter_counts, by = c("batter_name", "year"))

# ---- Sequencing test functions ----

run_seq_test <- function(df) {
  tryCatch({
    df$cluster <- droplevels(df$cluster)
    if (nrow(df) < 100 || length(levels(df$cluster)) < 2) return(tibble())
    
    model_seq <- ranger(
      cluster ~ pitch_hand + count + outs +
        prev_pitch_name + prev_swing + prev_zone + pitch_name + zone,
      num.trees = 500, importance = "impurity", probability = FALSE, data = df
    )
    model_no_seq <- ranger(
      cluster ~ pitch_hand + pitch_name + zone,
      num.trees = 500, importance = "impurity", probability = FALSE, data = df
    )
    
    tibble(
      acc_seq    = mean(model_seq$predictions == df$cluster, na.rm = TRUE),
      acc_no_seq = mean(model_no_seq$predictions == df$cluster, na.rm = TRUE),
      gain       = acc_seq - acc_no_seq,
      n          = nrow(df),
      n_clusters = length(levels(df$cluster))
    )
  }, error = function(e) tibble())
}

run_seq_test_v2 <- function(df) {
  tryCatch({
    df$cluster <- droplevels(df$cluster)
    if (nrow(df) < 100 || length(levels(df$cluster)) < 2) return(NULL)
    
    model_seq <- ranger(
      cluster ~ pitch_hand + count + outs +
        prev_pitch_name + prev_swing + prev_zone + pitch_name + zone,
      num.trees = 500, importance = "impurity", data = df
    )
    model_no_seq <- ranger(
      cluster ~ pitch_hand + pitch_name + zone,
      num.trees = 500, importance = "impurity", data = df
    )
    
    imp <- model_seq$variable.importance
    seq_vars <- c("prev_pitch_name", "prev_swing", "prev_zone")
    
    seq_importance <- tibble(variable = names(imp), importance = as.numeric(imp)) %>%
      arrange(desc(importance)) %>%
      mutate(rank = row_number()) %>%
      filter(variable %in% seq_vars)
    
    acc_seq    <- mean(model_seq$predictions == df$cluster, na.rm = TRUE)
    acc_no_seq <- mean(model_no_seq$predictions == df$cluster, na.rm = TRUE)
    
    cluster_gains <- map_dfr(levels(df$cluster), function(cl) {
      actual <- df$cluster == cl
      tibble(
        cluster      = cl,
        sens_seq     = mean(model_seq$predictions[actual] == cl, na.rm = TRUE),
        sens_no_seq  = mean(model_no_seq$predictions[actual] == cl, na.rm = TRUE),
        cluster_gain = sens_seq - sens_no_seq,
        cluster_n    = sum(actual)
      )
    })
    
    list(
      overall    = tibble(acc_seq, acc_no_seq, gain = acc_seq - acc_no_seq,
                          n = nrow(df), n_clusters = length(levels(df$cluster))),
      by_cluster = cluster_gains,
      importance = seq_importance
    )
  }, error = function(e) NULL)
}

# ---- Run across all hitters ----

swing_data <- filtered_data %>% filter(swing == TRUE, !is.na(cluster))

all_results <- swing_data %>%
  group_by(batter_name, year) %>%
  group_modify(~ { res <- run_seq_test_v2(.x); if (is.null(res)) tibble() else res$overall }) %>%
  ungroup()

importance_results <- swing_data %>%
  group_by(batter_name, year) %>%
  group_modify(~ { res <- run_seq_test_v2(.x); if (is.null(res)) tibble() else res$importance }) %>%
  ungroup()

cluster_summary <- swing_data %>%
  group_by(batter_name, year) %>%
  group_modify(~ { res <- run_seq_test_v2(.x); if (is.null(res)) tibble() else res$by_cluster }) %>%
  ungroup() %>%
  group_by(batter_name, year, cluster) %>%
  summarize(mean_gain = mean(cluster_gain, na.rm = TRUE),
            mean_n    = mean(cluster_n, na.rm = TRUE)) %>%
  arrange(desc(mean_gain))

centroids <- swing_data %>%
  mutate(contact = !(description %in% c("swinging_strike", "swinging_strike_blocked"))) %>%
  group_by(batter_name, year, cluster) %>%
  summarize(n = n(),
            across(c(bat_speed, swing_length, attack_angle, swing_path_tilt,
                     intercept_ball_minus_batter_pos_y_inches, squared_up,
                     expected_woba, launch_speed, contact),
                   mean, na.rm = TRUE))

merged <- all_results %>%
  left_join(
    importance_results %>%
      group_by(batter_name, year) %>%
      summarise(avg_seq_importance = mean(importance), .groups = "drop"),
    by = c("batter_name", "year")
  )

# ---- Results ----

all_results %>%
  summarize(mean_gain = mean(gain, na.rm = TRUE),
            pct_positive = mean(gain > 0, na.rm = TRUE))

importance_results %>%
  group_by(variable) %>%
  summarise(avg_rank = mean(rank, na.rm = TRUE),
            avg_importance = mean(importance, na.rm = TRUE))

total_gain <- cluster_summary %>%
  group_by(batter_name, year) %>%
  summarize(total = sum(mean_gain)) %>%
  ungroup()

total_gain %>% summarize(sum(total > 0))

# ---- Plots ----

lm_fit <- lm(gain ~ avg_seq_importance, data = merged)
r2 <- summary(lm_fit)$r.squared

ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sequencing Importance vs Predictive Gain",
       subtitle = paste0("R² = ", round(r2, 3)),
       x = "Average Sequencing Variable Importance",
       y = "Sequencing Accuracy Gain") +
  theme_minimal(base_size = 14)

ggplot(all_results, aes(x = factor(n_clusters), y = gain)) +
  geom_boxplot() +
  labs(x = "Number of Swing Clusters", y = "Sequencing Gain")

merged2 <- all_results %>%
  left_join(importance_results, by = c("batter_name", "year"))

merged2 %>%
  mutate(gain_direction = ifelse(gain > 0, "Positive gain", "Negative gain")) %>%
  group_by(variable, gain_direction) %>%
  summarise(avg_rank = mean(rank), .groups = "drop") %>%
  ggplot(aes(x = variable, y = avg_rank, fill = gain_direction)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Positive gain" = "#E24B4A", "Negative gain" = "#378ADD")) +
  labs(x = "Variable", y = "Average rank", fill = NULL) +
  theme_minimal()

summary(lm_fit)
cor(merged$avg_seq_importance, merged$gain, use = "complete.obs")

ggplot(all_results, aes(x = gain)) +
  geom_histogram(bins = 30, fill = "blue", color="black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of Gain",
    x = "Gain",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)
