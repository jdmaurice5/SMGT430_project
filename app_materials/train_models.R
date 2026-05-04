library(tidyverse)
library(ranger)

set.seed(42)

# ── 1. Load & prep data ────────────────────────────────────────────────────────
data <- read_csv("savant_data_with_clusters_and_preds.csv")

data <- data %>%
  arrange(game_id, at_bat_number, pitch_number) %>%
  group_by(game_id, at_bat_number) %>%
  mutate(
    prev_cluster   = lag(cluster),
    prev_swing     = lag(swing),
    prev_zone      = lag(zone),
    prev_pitch_name = lag(pitch_name)
  ) %>%
  ungroup() %>%
  mutate(
    cluster        = as.factor(cluster),
    prev_cluster   = as.factor(ifelse(is.na(prev_cluster), "None", as.character(prev_cluster))),
    prev_swing     = ifelse(is.na(prev_swing), 0, prev_swing),
    zone           = as.factor(zone),
    prev_zone      = as.factor(ifelse(is.na(prev_zone), "None", as.character(prev_zone)))
  )

# ── 2. Top 100 hitters by total swings (all years combined) ───────────────────
top_hitters <- data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  count(batter_name, name = "n_swings") %>%
  slice_max(n_swings, n = 100) %>%
  pull(batter_name)

filtered_data <- data %>%
  filter(batter_name %in% top_hitters)

cat("Top 100 hitters selected. Total rows:", nrow(filtered_data), "\n")

# ── 3. Save metadata: unique values for UI dropdowns ──────────────────────────
metadata <- list(
  batters     = sort(top_hitters),
  pitch_names = sort(unique(na.omit(data$pitch_name))),
  prev_pitch_names = sort(unique(na.omit(data$prev_pitch_name))),
  zones       = sort(unique(na.omit(as.integer(as.character(data$zone))))),
  counts      = sort(unique(na.omit(data$count))),
  clusters    = sort(unique(na.omit(as.integer(as.character(data$cluster)))))
)

# ── 4. Compute cluster centroids (all years combined, per batter) ─────────────
centroids <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  mutate(contact = !(description %in% c("swinging_strike", "swinging_strike_blocked"))) %>%
  group_by(batter_name, cluster) %>%
  summarize(
    n                                          = n(),
    bat_speed                                  = mean(bat_speed,                                  na.rm = TRUE),
    swing_length                               = mean(swing_length,                               na.rm = TRUE),
    attack_angle                               = mean(attack_angle,                               na.rm = TRUE),
    swing_path_tilt                            = mean(swing_path_tilt,                            na.rm = TRUE),
    intercept_ball_minus_batter_pos_y_inches   = mean(intercept_ball_minus_batter_pos_y_inches,   na.rm = TRUE),
    squared_up                                 = mean(squared_up,                                 na.rm = TRUE),
    expected_woba                              = mean(expected_woba,                              na.rm = TRUE),
    launch_speed                               = mean(launch_speed,                               na.rm = TRUE),
    contact_rate                               = mean(contact,                                    na.rm = TRUE),
    .groups = "drop"
  )

# ── 5. Train one ranger model per batter ──────────────────────────────────────
model_formula <- cluster ~ pitch_hand + count + outs +
  prev_pitch_name + prev_swing + prev_zone +
  pitch_name + zone

all_models <- list()

for (b in top_hitters) {
  cat("Training:", b, "\n")
  
  batter_df <- filtered_data %>%
    filter(batter_name == b, swing == TRUE, !is.na(cluster)) %>%
    mutate(cluster = droplevels(cluster))
  
  # Need enough data and at least 2 clusters
  if (nrow(batter_df) < 100 || length(levels(batter_df$cluster)) < 2) {
    cat("  Skipping", b, "— insufficient data\n")
    next
  }
  
  # Ensure all factor levels are consistent with full dataset for prediction
  batter_df <- batter_df %>%
    mutate(
      zone           = factor(zone,           levels = as.character(metadata$zones)),
      prev_zone      = factor(prev_zone,      levels = c("None", as.character(metadata$zones))),
      pitch_name     = factor(pitch_name,     levels = metadata$pitch_names),
      prev_pitch_name = factor(prev_pitch_name, levels = c("None", metadata$prev_pitch_names))
    )
  
  model <- tryCatch({
    ranger(
      model_formula,
      data        = batter_df,
      num.trees   = 200,
      importance  = "impurity",
      probability = TRUE,   # gives P(cluster) per observation
      seed        = 42
    )
  }, error = function(e) {
    cat("  Error on", b, ":", conditionMessage(e), "\n")
    NULL
  })
  
  if (!is.null(model)) {
    all_models[[b]] <- model
  }
}

cat("\nModels trained for", length(all_models), "hitters.\n")

# ── 6. Bundle and save ────────────────────────────────────────────────────────
bundle <- list(
  models   = all_models,
  metadata = metadata,
  centroids = centroids
)

saveRDS(bundle, "shiny_app/bundle.rds")
cat("Bundle saved to shiny_app/bundle.rds\n")
cat("Estimated size:", round(file.size("shiny_app/bundle.rds") / 1e6, 1), "MB\n")
