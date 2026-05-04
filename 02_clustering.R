library(tidyverse)
library(mclust)
library(factoextra)
library(stringr)
library(patchwork)

theme_set(theme_light())

# ---- Clustering function ----

plot_player_clusters <- function(data, player_name) {
  
  name_parts <- str_split(player_name, ",\\s*")[[1]]
  name_title <- paste(name_parts[2], name_parts[1])
  
  player <- data %>%
    filter(batter_name == player_name) %>%
    mutate(row_id = row_number())
  
  player_clean <- player %>%
    filter(
      !(stringr::str_detect(description, "bunt") |
          (description == "hit_into_play" & stringr::str_detect(des, " bunt"))),
      bat_speed > 50
    ) %>%
    select(row_id, attack_direction, swing_path_tilt, attack_angle,
           bat_speed, swing_length, intercept_ball_minus_batter_pos_y_inches) %>%
    filter(if_all(-row_id, ~ !is.na(.) & is.finite(.)))
  
  player_mclust <- Mclust(player_clean %>% select(-row_id, -attack_direction))
  
  player_clean <- player_clean %>%
    mutate(cluster = as.factor(player_mclust$classification))
  
  player <- left_join(player, player_clean %>% select(row_id, cluster), by = "row_id")
  
  player <- player %>%
    mutate(
      runner_scoring_position = runner_on_second | runner_on_third,
      sac_fly_op     = runner_on_third & outs < 2,
      late_sac_fly_op = inning >= 7 & bat_score_diff %in% c(-1, 0) &
        runner_on_third & outs < 2,
      leverage_group = case_when(
        balls > strikes  ~ "Ahead",
        balls == strikes ~ "Even",
        balls < strikes  ~ "Behind",
        TRUE             ~ NA_character_
      ),
      zone_height     = strike_zone_top - strike_zone_bottom,
      vertical_zone   = case_when(
        plate_z > strike_zone_top - (1/3 * zone_height)    ~ "high",
        plate_z < strike_zone_bottom + (1/3 * zone_height) ~ "low",
        TRUE                                                ~ "middle"
      ),
      horizontal_zone = case_when(
        plate_x < -0.71 + (1/3 * 1.42) ~ "inside",
        plate_x >  0.71 - (1/3 * 1.42) ~ "outside",
        TRUE                            ~ "middle"
      ),
      location_zone = paste(vertical_zone, horizontal_zone, sep = "-"),
      strike_group  = ifelse(strikes == 2, "2 Strikes", "<2 Strikes")
    )
  
  # PCA
  pca_model   <- prcomp(player_clean %>% select(-row_id, -cluster), scale. = TRUE)
  pc_var      <- pca_model$sdev^2
  pc_var_exp  <- pc_var / sum(pc_var)
  pc1_label   <- paste0("PC1 (", round(pc_var_exp[1] * 100, 1), "%)")
  pc2_label   <- paste0("PC2 (", round(pc_var_exp[2] * 100, 1), "%)")
  
  player_clean <- player_clean %>%
    mutate(PC1 = pca_model$x[, 1], PC2 = pca_model$x[, 2])
  
  player <- left_join(player, player_clean %>% select(row_id, PC1, PC2), by = "row_id") %>%
    filter(!is.na(cluster))
  
  # Summaries
  contact_by_cluster <- player %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
      blast_pct      = mean(blast == TRUE, na.rm = TRUE),
      contact_pct    = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
      two_strike_contact_pct = mean(strikes == 2 & !(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE) /
        mean(strikes == 2, na.rm = TRUE),
      xwoba          = mean(expected_woba, na.rm = TRUE),
      swing_length   = mean(swing_length, na.rm = TRUE),
      swing_path_tilt = mean(swing_path_tilt, na.rm = TRUE),
      attack_angle   = mean(attack_angle, na.rm = TRUE),
      intercept      = mean(intercept_ball_minus_batter_pos_y_inches, na.rm = TRUE),
      bat_speed      = mean(bat_speed, na.rm = TRUE),
      .groups = "drop"
    )
  
  swing_reference <- player %>%
    filter(description %in% c("swinging_strike", "swinging_strike_blocked",
                              "foul", "foul_tip", "hit_into_play",
                              "hit_into_play_score", "hit_into_play_no_out")) %>%
    select(row_id, cluster, game_id, game_date, at_bat_number, pitch_number,
           batter_name, pitcher_id, pitch_name, description, events,
           launch_angle, launch_speed, plate_x, plate_z,
           sac_fly_op, late_sac_fly_op, strikes, balls, outs, inning, inning_topbot) %>%
    arrange(game_date, at_bat_number, pitch_number)
  
  return(list(
    swing_reference    = swing_reference,
    contact_by_cluster = contact_by_cluster,
    pca_loadings       = pca_model$rotation,
    pca_summary        = summary(pca_model)
  ))
}

# ---- Run for all players, both years ----

data_2024 <- read_csv("data/savant_data_clean_2024.csv")
data_2025 <- read_csv("data/savant_data_clean_2025.csv")

players <- bind_rows(data_2024, data_2025) %>% pull(batter_name) %>% unique()

# 2024
player_results_2024 <- list()
problem_players_2024 <- c()

for (player in players) {
  result <- tryCatch(
    plot_player_clusters(data_2024, player),
    error = function(e) {
      message("Skipping ", player, ": ", e$message)
      problem_players_2024 <<- c(problem_players_2024, player)
      NULL
    }
  )
  if (!is.null(result)) player_results_2024[[player]] <- result
}

# 2025
player_results_2025 <- list()
problem_players_2025 <- c()

for (player in players) {
  result <- tryCatch(
    plot_player_clusters(data_2025, player),
    error = function(e) {
      message("Skipping ", player, ": ", e$message)
      problem_players_2025 <<- c(problem_players_2025, player)
      NULL
    }
  )
  if (!is.null(result)) player_results_2025[[player]] <- result
}

# ---- Join clusters back and write ----

build_clustered_data <- function(data, player_results) {
  swing_ref_clean <- bind_rows(lapply(player_results, function(x) x$swing_reference)) %>%
    select(game_id, inning, pitch_number, at_bat_number, batter_name, cluster) %>%
    distinct()
  
  data %>%
    left_join(swing_ref_clean,
              by = c("game_id", "inning", "pitch_number", "at_bat_number", "batter_name"))
}

data_2024_with_clusters <- build_clustered_data(data_2024, player_results_2024)
data_2025_with_clusters <- build_clustered_data(data_2025, player_results_2025)

write_csv(data_2024_with_clusters, "data/savant_data_with_clusters_2024.csv")
write_csv(data_2025_with_clusters, "data/savant_data_with_clusters_2025.csv")

combined_data <- bind_rows(data_2024_with_clusters, data_2025_with_clusters)
write_csv(combined_data, "data/savant_data_with_clusters_2024_2025.csv")