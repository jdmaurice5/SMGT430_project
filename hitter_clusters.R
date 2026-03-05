set.seed(42)
library(mclust)
library(ggplot2)
library(dplyr)
library(ggmosaic)
library(factoextra)
library(stringr)
library(tidyverse)
library(patchwork)
theme_set(theme_light())

data_2024=read_csv("savant_data_2024.csv")
data_2025=read_csv("savant_data_2025.csv")


clean_data <- function(data) {


  # remove_partial_swings <- function(swing) {
  #   
  #   swing_filtered <- swing |>
  #     dplyr::filter(
  #       # remove bunt attempts
  #       !(
  #         stringr::str_detect(description, "bunt") |  # only detects missed and foul bunt attempts
  #           (description == "hit_into_play" & stringr::str_detect(des, " bunt"))  # covers fair bunts
  #       ),
  #       # remove checked swings (which only count as swings if they accidentally result in contact)
  #       bat_speed > 50  # this seemingly arbitrary cutoff is the result of extensive EDA
  #     )
  #   
  #   return(swing_filtered)
  # }
  # 
  # data=remove_partial_swings(data)

  data <- data |>
  sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
  sabRmetrics::get_trackman_metrics()

  recreate_squared_up <- function(data) {
    
    data_enhanced <- data |>
      dplyr::mutate(
        plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
        plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
        plate_speed = 0.6818182 * sqrt(
          (ax * plate_time + bx)^2 + (ay * plate_time + by)^2 + (az * plate_time + bz)^2
        ),
        squared_up = ifelse(
          test = description == "hit_into_play" & !is.na(launch_speed),
          yes = (launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) > 0.8,
          no = FALSE
        )
      ) |>
      dplyr::select(dplyr::all_of(colnames(data)), squared_up) # drop intermediate columns
    
    return(data_enhanced)
  }

  data=recreate_squared_up(data)

  recreate_blasts <- function(data) {
    
    data_enhanced <- data |>
      dplyr::mutate(
        plate_y = 17 / 12,  # back of home plate is zero; front is 17 inches
        plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
        plate_speed = 0.6818182 * sqrt(
          (ax * plate_time + bx)^2 + (ay * plate_time + by)^2 + (az * plate_time + bz)^2
        ),
        blast = ifelse(
          test = description == "hit_into_play" & !is.na(launch_speed),
          yes = ((launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) * 100)+bat_speed>=164,
          no = FALSE
        )
      ) |>
      dplyr::select(dplyr::all_of(colnames(data)), blast) # drop intermediate columns
    
    return(data_enhanced)
  }

  data=recreate_blasts(data)

  data = data %>% mutate(outcome=case_when(
    events %in% c("fielders_choice", "field_out", 
                  "fielders_choice_out", "force_out", "double_play",
                  "triple_play", "strikeout", "grounded_into_double_play",
                  "field_error", "strikeout_double_play") ~ "out",
    events %in% c("single", "home_run", "double", "triple") ~ "hit",
    events %in% c("walk", "hit_by_pitch", "intent_walk") ~ "walk",
    events %in% c("sac_fly", "sac_fly_double_play", "sac_bunt") ~ "sacrifice",
    TRUE ~ "other"
  ))
  
  data=data %>% mutate(middle=(strike_zone_bottom+strike_zone_top)/2) 
  
  swing_attempt= c("foul", "hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip")
  
  data=data %>% mutate(swing=description %in% swing_attempt)
  
  data=data %>% mutate(ideal_attack_angle = ifelse(attack_angle>=5 & attack_angle<=20, 1, 0)) %>%
    mutate(ideal_attack_angle=as_factor(ideal_attack_angle))
  
  
  data <- data %>%
    mutate(
      runner_on_first = !is.na(pre_runner_1b_id),
      runner_on_second = !is.na(pre_runner_2b_id),
      runner_on_third = !is.na(pre_runner_3b_id),
      base_state = case_when(
        runner_on_first & runner_on_second & runner_on_third ~ "1B_2B_3B",
        runner_on_first & runner_on_second ~ "1B_2B",
        runner_on_first & runner_on_third ~ "1B_3B",
        runner_on_second & runner_on_third ~ "2B_3B",
        runner_on_first ~ "1B",
        runner_on_second ~ "2B",
        runner_on_third ~ "3B",
        TRUE ~ "Empty"
      ),
      game_state = paste0(base_state, " | ", outs, " Outs")
    )

  return(data)
}

data_2024 = clean_data(data_2024)

data_2025= clean_data(data_2025)



plot_player_clusters <- function(data, player_name) {
  
  # Format player name for titles
  name_parts <- str_split(player_name, ",\\s*")[[1]]
  name_title <- paste(name_parts[2], name_parts[1])
  
  # Filter player data and add row_id for tracking rows
  player <- data %>%
    filter(batter_name == player_name) %>%
    mutate(row_id = row_number())  # <--- Important: add row_id here
  
  # Prepare data for clustering: keep row_id to join later
  player_clean <- player %>%

    filter(!(
            stringr::str_detect(description, "bunt") |  # only detects missed and foul bunt attempts
              (description == "hit_into_play" & stringr::str_detect(des, " bunt"))  # covers fair bunts
          ),
          # remove checked swings (which only count as swings if they accidentally result in contact)
          bat_speed > 50  # this seemingly arbitrary cutoff is the result of extensive EDA
        ) %>% 
    select(row_id, attack_direction, swing_path_tilt, attack_angle, bat_speed, swing_length, intercept_ball_minus_batter_pos_y_inches) %>%
    filter(if_all(-row_id, ~ !is.na(.) & is.finite(.)))
  
  # Run Mclust on player_clean (without row_id)
  player_mclust <- Mclust(player_clean %>% select(-row_id, -attack_direction))
  
  # Add cluster to player_clean (same order as clustering)
  player_clean <- player_clean %>%
    mutate(cluster = as.factor(player_mclust$classification))
  
  # Join clusters back to full player data using row_id
  player <- left_join(player, player_clean %>% select(row_id, cluster), by = "row_id")
  
  # Add scoring situation flags to player
  player <- player %>%
    mutate(
      runner_scoring_position = runner_on_second == TRUE | runner_on_third == TRUE,
      sac_fly_op = runner_on_third == TRUE & outs < 2,
      late_sac_fly_op = inning >= 7 &
        bat_score_diff %in% c(-1, 0) &
        runner_on_third == TRUE &
        outs < 2
    ) %>% 
    mutate(
      leverage_group = case_when(
        balls > strikes ~ "Ahead",
        balls == strikes ~ "Even",
        balls < strikes ~ "Behind",
        TRUE ~ NA_character_
      )
    )
  
  
  plot_leverage <- ggplot(player %>%
                            filter(!is.na(cluster), !is.na(leverage_group))) +
    geom_bar(aes(x = leverage_group, fill = cluster), position = "fill") +
    labs(title = paste0(name_title, " - Cluster vs. Count Leverage Group"),
         x = "Cluster × Count Leverage",
         y = "Proportion") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.text.y = element_blank()
    )
  # Mosaic Plots (using player with cluster)
  plot1 <- ggplot() +
    geom_bar(data=player,
             aes(x = runner_scoring_position, fill = cluster), position = "fill") +
    ggtitle(paste(name_title, "- Cluster vs. RSP"))
  
  plot2 <- ggplot(player) +
    geom_bar(aes(x = sac_fly_op, fill = cluster), position = "fill") +
    ggtitle(paste(name_title, "- Cluster vs. Sac Fly Opportunity"))
  
  plot3 <- ggplot(player) +
    geom_bar(aes(x = strikes, fill = cluster), position = "fill") +
    ggtitle(paste(name_title, "- Cluster vs. Strikes"))
  
  plot5 <- ggplot(player) +
    geom_bar(aes(x = late_sac_fly_op, fill = cluster), position = "fill") +
    ggtitle(paste(name_title, "- Cluster vs. Late Close Sac Fly Opportunity"))
  
  # Summary: % squared up and blast overall
  squared_up_pct <- mean(player$squared_up == TRUE, na.rm = TRUE)
  blast_pct <- mean(player$blast == TRUE, na.rm = TRUE)
  
  cat(paste0("\n", name_title, " - Quality Contact Summary:\n"))
  cat(sprintf("  • %% Squared Up: %.1f%%\n", squared_up_pct * 100))
  cat(sprintf("  • %% Blast: %.1f%%\n", blast_pct * 100))
  
  # Launch angle summary by cluster
  avg_launch_by_cluster <- player %>%
    filter(!is.na(launch_angle)) %>%
    group_by(cluster) %>%
    summarise(avg_launch_angle = mean(launch_angle), .groups = "drop")
  
  cat("\nAverage Launch Angle by Cluster:\n")
  print(avg_launch_by_cluster)
  
  # Launch angle density plot
  plot4 <- player %>%
    filter(!is.na(launch_angle)) %>%
    ggplot(aes(x = launch_angle, fill = cluster)) +
    geom_density(alpha = 0.4) +
    ggtitle(paste(name_title, "- Launch Angle by Cluster")) +
    theme_minimal()
  
  # Contact quality summary by cluster
  contact_by_cluster <- player %>%
    group_by(cluster) %>%
    summarise(
      n = n(),
      squared_up_pct = mean(squared_up == TRUE, na.rm = TRUE),
      blast_pct = mean(blast == TRUE, na.rm = TRUE),
      contact_pct = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
      two_strike_contact_pct = mean(strikes == 2 & !(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE) /
        mean(strikes == 2, na.rm = TRUE),
      xwoba = mean(expected_woba, na.rm = TRUE),
      swing_length = mean(swing_length, na.rm=TRUE),
      swing_path_tilt=mean(swing_path_tilt, na.rm=TRUE),
      attack_angle=mean(attack_angle, na.rm=TRUE),
      intercept=mean(intercept_ball_minus_batter_pos_y_inches, na.rm=TRUE),
      bat_speed=mean(bat_speed, na.rm=TRUE),
      .groups = "drop"
    )
  
  cat("\nContact Quality by Cluster:\n")
  print(contact_by_cluster)
  
  # Zone location calculation
  player <- player %>%
    mutate(
      zone_height = strike_zone_top - strike_zone_bottom,
      vertical_zone = case_when(
        plate_z > strike_zone_top - (1 / 3 * zone_height) ~ "high",
        plate_z < strike_zone_bottom + (1 / 3 * zone_height) ~ "low",
        TRUE ~ "middle"
      ),
      horizontal_zone = case_when(
        plate_x < -0.71 + (1 / 3 * 1.42) ~ "inside",
        plate_x > 0.71 - (1 / 3 * 1.42) ~ "outside",
        TRUE ~ "middle"
      ),
      location_zone = paste(vertical_zone, horizontal_zone, sep = "-"),
      strike_group = ifelse(strikes == 2, "2 Strikes", "<2 Strikes")
    )
  
  # Mosaic plot: Pitch Type vs Cluster (2-strike counts only)
  plot6 <- player %>%
    filter(strikes == 2, !is.na(pitch_name)) %>%
    add_count(pitch_name, name = "pitch_count") %>%
    filter(pitch_count >= 20) %>%
    ggplot() +
    geom_bar(aes(x = pitch_name, fill = cluster), position = "fill") +
    ggtitle(paste(name_title, "- Cluster vs. Pitch Type (2 Strikes, Min 20 Pitches)")) +
    labs(x = "Cluster × Pitch Type",
         y = "Proportion") +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 18, face = "bold"),  # Title size
      axis.title.x = element_text(size = 14),               # X-axis label size
      axis.title.y = element_text(size = 14),               # Y-axis label size
      axis.text.x = element_text(size = 10),
      legend.title = element_text(size = 14),     # Legend title size
      legend.text = element_text(size = 12),
      axis.text.y = element_blank()
    )
  
  plot8 <- ggplot(player %>%
                    filter(!is.na(cluster), !is.na(strikes))) +
    geom_bar(aes(x = clutch_situation, fill = cluster), position = "fill") +
    ggtitle(paste0(name_title, " - Cluster vs. Clutch Situation Group")) +
    xlab("Clutch Situation") +
    ylab("Proportion") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
  
  # Mosaic plot: Location Zone vs Cluster (2-strike counts only)
  plot7 <- player %>%
    filter(strikes == 2) %>%
    ggplot() +
    geom_bar(aes(x = location_zone, fill = cluster), position="fill") +
    ggtitle(paste(name_title, "- Cluster vs. Location (2 Strikes)"))
  
  # --- PCA Section ---
  # Run PCA on cleaned data (exclude row_id and cluster)
  pca_model <- prcomp(player_clean %>% select(-row_id, -cluster), scale. = TRUE)
  
  # Add PC1 and PC2 scores to player_clean
  player_clean <- player_clean %>%
    mutate(
      PC1 = pca_model$x[, 1],
      PC2 = pca_model$x[, 2]
    )
  
  # Merge PC1 and PC2 back into full player data via row_id
  player <- left_join(player, player_clean %>% select(row_id, PC1, PC2), by = "row_id")
  
  # Optionally filter to keep only clustered rows
  player <- player %>% filter(!is.na(cluster))
  
  # Calculate variance explained labels for axes
  pc_var <- pca_model$sdev^2
  pc_var_exp <- pc_var / sum(pc_var)
  pc1_label <- paste0("PC1 (", round(pc_var_exp[1] * 100, 1), "%)")
  pc2_label <- paste0("PC2 (", round(pc_var_exp[2] * 100, 1), "%)")
  
  # PCA scatter plot colored by cluster
  pca_cluster_plot <- ggplot(player_clean, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(alpha = 0.6, size = 2) +
    theme_minimal() +
    labs(
      title = paste(name_title, "- PCA by Cluster"),
      x = pc1_label,
      y = pc2_label,
      color = "Cluster"
    ) +
    scale_color_brewer(palette = "Set1")
  
  # PCA biplot using factoextra (optional)
  pca_plot <- fviz_pca_biplot(
    pca_model,
    label = "var",
    alpha.ind = 0.25,
    alpha.var = 0.75,
    labelsize = 5,
    col.var = "darkblue",
    repel = TRUE,
    title = paste(name_title, "PCA - Biplot")
  )
  
  swing_reference <- player %>%
    filter(description %in% c(
      "swinging_strike", "swinging_strike_blocked",
      "foul", "foul_tip",
      "hit_into_play", "hit_into_play_score", "hit_into_play_no_out"
    )) %>%
    select(
      row_id, cluster,
      game_id, game_date, at_bat_number, pitch_number,
      batter_name, pitcher_id, pitch_name,
      description, events,
      launch_angle, launch_speed,
      plate_x, plate_z,
      sac_fly_op, late_sac_fly_op,
      strikes, balls, outs, inning, inning_topbot
    ) %>%
    arrange(game_date, at_bat_number, pitch_number)
  
  cat("\nSample Swing Reference Table:\n")
  print(head(swing_reference, 10))
  
  # Return plots and summaries
  return(list(
    swing_reference = swing_reference,
    rsp_plot = plot1,
    sac_fly_plot = plot2,
    strikes_plot = plot3,
    launch_plot = plot4,
    late_sac_plot = plot5,
    pitch_type_2s_plot = plot6,
    location_2s_plot = plot7,
    plot8=plot8,
    plot_leverage=plot_leverage,
    avg_launch_df = avg_launch_by_cluster,
    contact_by_cluster = contact_by_cluster,
    pca_plot = pca_plot,
    pca_cluster_plot = pca_cluster_plot,
    pca_summary = summary(pca_model),
    pca_loadings = pca_model$rotation
  ))
}


player_results <- list()
problem_players <- c()

# Pull all unique players
players <- data_2024 %>%
  bind_rows(data_2025) %>%
  pull(batter_name) %>%
  unique()

# If player_results already has some completed players, skip them
remaining_players <- setdiff(players, names(player_results))

# Loop over remaining players
for(player in remaining_players){
  result <- tryCatch(
    {
      # Run the function
      plot_player_clusters(data_2024, player)
    },
    error = function(e){
      # Catch errors and store player name
      message("Skipping player ", player, ": ", e$message)
      problem_players <<- c(problem_players, player)
      return(NULL)
    }
  )
  
  # Store the result if successful
  if(!is.null(result)){
    player_results[[player]] <- result
  }
}

# After loop, you can see problematic players
problem_players

problem_sizes <- data_2024 %>%
  filter(batter_name %in% problem_players) %>%
  group_by(batter_name) %>%
  summarise(n_pitches = n(), .groups = "drop")

# Summary statistics
summary(problem_sizes$n_pitches)

# Or get quantiles explicitly
quantile(problem_sizes$n_pitches, probs = c(0, 0.25, 0.5, 0.75, 1))


players=(data_2024 %>% rbind(data_2025) %>% pull(batter_name) %>%
           unique())



swing_ref_all <- bind_rows(
  lapply(player_results, function(x) x$swing_reference)
)

swing_ref_clean <- swing_ref_all %>% 
  select(
    game_id,
    inning,
    pitch_number,
    at_bat_number,
    batter_name,
    cluster
  ) %>% 
  distinct()

data_2024_with_clusters <- data_2024 %>% 
  left_join(
    swing_ref_clean,
    by = c(
      "game_id",
      "inning",
      "pitch_number",
      "at_bat_number",
      "batter_name"
    )
  )

nrow(data_2024_with_clusters) == nrow(data_2024)

data_2024_with_clusters %>% 
  filter(!is.na(cluster)) %>% 
  count(description)

write_csv(data_2024_with_clusters, "savant_data_with_clusters_2024.csv")

#######################

player_results_2025 <- list()
problem_players_2025 <- c()



# If player_results already has some completed players, skip them
remaining_players <- setdiff(players, names(player_results_2025))

# Loop over remaining players
for(player in remaining_players){
  result <- tryCatch(
    {
      # Run the function
      plot_player_clusters(data_2025, player)
    },
    error = function(e){
      # Catch errors and store player name
      message("Skipping player ", player, ": ", e$message)
      problem_players <<- c(problem_players, player)
      return(NULL)
    }
  )
  
  # Store the result if successful
  if(!is.null(result)){
    player_results_2025[[player]] <- result
  }
}

# After loop, you can see problematic players
problem_players_2025

problem_sizes <- data_2024 %>%
  filter(batter_name %in% problem_players) %>%
  group_by(batter_name) %>%
  summarise(n_pitches = n(), .groups = "drop")

test <- plot_player_clusters(data_2025, players[1])
print(test)

# Summary statistics
summary(problem_sizes$n_pitches)

# Or get quantiles explicitly
quantile(problem_sizes$n_pitches, probs = c(0, 0.25, 0.5, 0.75, 1))


players=(data_2024 %>% rbind(data_2025) %>% pull(batter_name) %>%
           unique())



swing_ref_all <- bind_rows(
  lapply(player_results_2025, function(x) x$swing_reference)
)

swing_ref_clean <- swing_ref_all %>% 
  select(
    game_id,
    inning,
    pitch_number,
    at_bat_number,
    batter_name,
    cluster
  ) %>% 
  distinct()

data_2025_with_clusters <- data_2025 %>% 
  left_join(
    swing_ref_clean,
    by = c(
      "game_id",
      "inning",
      "pitch_number",
      "at_bat_number",
      "batter_name"
    )
  )

nrow(data_2025_with_clusters) == nrow(data_2025)

data_2025_with_clusters %>% 
  filter(!is.na(cluster)) %>% 
  count(description)


write_csv(data_2025_with_clusters, "savant_data_with_clusters_2025.csv")

combined_data = data_2024_with_clusters %>% rbind(data_2025_with_clusters)


write_csv(combined_data, "savant_data_with_clusters_2024_2025.csv")








