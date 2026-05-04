library(sabRmetrics)
library(tidyverse)
cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2024-01-01",
  end_date = "2024-12-31",
  cl = cluster
)
parallel::stopCluster(cluster)

write_csv(data_baseballsavant, "savant_data_2024.csv")


cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2025-01-01",
  end_date = "2025-12-31",
  cl = cluster
)
parallel::stopCluster(cluster)

write_csv(data_baseballsavant, "savant_data_2025.csv")


data_baseballsavant_2024 = read_csv("savant_data_2024.csv")
data_baseballsavant_2025 = read_csv("savant_data_2025.csv")

data_baseballsavant_2024=data_baseballsavant_2024 %>% mutate(middle=(strike_zone_bottom+strike_zone_top)/2) 

swing= c("foul", "hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip")

swing_data_2024=data_baseballsavant_2024 %>% filter(description %in% swing)

swing_data_2024=swing_data_2024 %>% mutate(ideal_attack_angle = ifelse(attack_angle>=5 & attack_angle<=20, 1, 0)) %>%
  mutate(ideal_attack_angle=as_factor(ideal_attack_angle))


remove_partial_swings <- function(swing) {
  
  swing_filtered <- swing |>
    dplyr::filter(
      # remove bunt attempts
      !(
        stringr::str_detect(description, "bunt") |  # only detects missed and foul bunt attempts
          (description == "hit_into_play" & stringr::str_detect(des, " bunt"))  # covers fair bunts
      ),
      # remove checked swings (which only count as swings if they accidentally result in contact)
      bat_speed > 50  # this seemingly arbitrary cutoff is the result of extensive EDA
    )
  
  return(swing_filtered)
}

swing_data_2024=remove_partial_swings(swing_data_2024)

swing_data_2024 <- swing_data_2024 |>
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

swing_data_2024=recreate_squared_up(swing_data_2024)

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

swing_data_2024=recreate_blasts(swing_data_2024)


swing_data_2024 <- swing_data_2024 %>%
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

write_csv(swing_data_2024, "swing_data_2024.csv")

##########

data_baseballsavant_2025=data_baseballsavant_2025 %>% mutate(middle=(strike_zone_bottom+strike_zone_top)/2) 

swing= c("foul", "hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip")

swing_data_2025=data_baseballsavant_2025 %>% filter(description %in% swing)

swing_data_2025=swing_data_2025 %>% mutate(ideal_attack_angle = ifelse(attack_angle>=5 & attack_angle<=20, 1, 0)) %>%
  mutate(ideal_attack_angle=as_factor(ideal_attack_angle))


remove_partial_swings <- function(swing) {
  
  swing_filtered <- swing |>
    dplyr::filter(
      # remove bunt attempts
      !(
        stringr::str_detect(description, "bunt") |  # only detects missed and foul bunt attempts
          (description == "hit_into_play" & stringr::str_detect(des, " bunt"))  # covers fair bunts
      ),
      # remove checked swings (which only count as swings if they accidentally result in contact)
      bat_speed > 50  # this seemingly arbitrary cutoff is the result of extensive EDA
    )
  
  return(swing_filtered)
}

swing_data_2025=remove_partial_swings(swing_data_2025)

swing_data_2025 <- swing_data_2025 |>
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

swing_data_2025=recreate_squared_up(swing_data_2025)

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

swing_data_2025=recreate_blasts(swing_data_2025)


swing_data_2025 <- swing_data_2025 %>%
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

write_csv(swing_data_2025, "swing_data_2025.csv")



library(mclust)
library(ggplot2)
library(dplyr)
library(ggmosaic)
library(factoextra)
library(stringr)
library(tidyverse)
library(patchwork)
theme_set(theme_light())

swing_data_2025 = read_csv("swing_data_2025.csv")


swing_data_2025 %>% 
  ggplot(aes(x=attack_direction, y=intercept_ball_minus_batter_pos_y_inches))+
  geom_point()


judge = swing_data_2025 %>% filter(batter_name=="Judge, Aaron")

judge %>% 
  ggplot(aes(x=attack_direction, y=intercept_ball_minus_batter_pos_y_inches, 
             colour = blast, alpha = 0.7))+
  geom_point()

judge %>% 
  ggplot(aes(x=bat_speed, y=swing_length, 
             colour = blast, alpha = 0.7))+
  geom_point()

arraez = swing_data_2025 %>% filter(batter_name=="Arraez, Luis")

arraez %>% 
  ggplot(aes(x=attack_direction, y=intercept_ball_minus_batter_pos_y_inches, 
             colour = blast, alpha = 0.7))+
  geom_point()

arraez %>% 
  ggplot(aes(x=bat_speed, y=swing_length, 
             colour = blast, alpha = 0.7))+
  geom_point()

ohtani = swing_data_2025 %>% filter(batter_name=="Ohtani, Shohei", attack_direction<100)

ohtani %>% 
  ggplot(aes(x=attack_direction, y=intercept_ball_minus_batter_pos_y_inches, 
             colour = blast, alpha = 0.7))+
  geom_point()

ohtani %>% 
  ggplot(aes(x=bat_speed, y=swing_length, 
             colour = blast, alpha = 0.7))+
  geom_point()

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
    geom_bar(aes(x = strike_group, fill = cluster), position = "fill") +
    ggtitle(paste0(name_title, " - Cluster vs. Strike Count Group")) +
    xlab("Strike Group") +
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


plots <- plot_player_clusters(swing_data_2025, "Altuve, Jose")

altuve_swings=plots$swing_reference

all_data_2025=read_csv("savant_data_2025.csv")

all_altuve = all_data_2025 %>% filter(batter_name=="Altuve, Jose")

altuve_full <- all_altuve %>%
  left_join(
    altuve_swings %>% select(game_date, at_bat_number, pitch_number, cluster, description, events),
    by = c("game_date", "at_bat_number", "pitch_number"),
    suffix = c("", "_swing")
  )


# See what pitch preceded a swing in a given cluster
altuve_full %>%
  arrange(game_date, at_bat_number, pitch_number) %>%
  group_by(game_date, at_bat_number) %>%
  mutate(
    next_cluster = lead(cluster),
    next_description = lead(description)
  ) %>%
  filter(!is.na(next_cluster)) %>%  # rows where the NEXT pitch was a swing
  select(game_date, at_bat_number, pitch_number, pitch_name, 
         plate_x, plate_z, next_cluster, next_description)


plots$plot8
plots$sac_fly_plot
plots$strikes_plot
plots$launch_plot
plots$plot_leverage
plots$avg_launch_df
x=plots$contact_by_cluster
plots$pitch_type_2s_plot
plots$location_2s_plot
plots$plot8
plots$pca_plot
plots$pca_cluster_plot
plots$pca_summary
x

cluster3_sequences <- altuve_full %>%
  arrange(game_date, at_bat_number, pitch_number) %>%
  group_by(game_date, at_bat_number) %>%
  mutate(
    prev_pitch_name = lag(pitch_name),
    prev_zone       = lag(zone),
    prev_description = lag(description)
  ) %>%
  ungroup() %>%
  filter(cluster == "3") %>%
  select(
    game_date, at_bat_number, pitch_number,
    prev_pitch_name, prev_zone, prev_description,
    pitch_name, zone, cluster,
    description, events,
    balls, strikes, outs, inning
  )

print(cluster3_sequences)

cluster3_sequences %>%
  filter(!is.na(prev_pitch_name)) %>%  # drops cases where swing was pitch 1 of AB
  count(prev_pitch_name, pitch_name, name = "n") %>%
  arrange(desc(n)) %>%
  print(n = 30)




compare_pca_clusters <- function(data, player_name) {
  
  name_parts <- str_split(player_name, ",\\s*")[[1]]
  name_title <- paste(name_parts[2], name_parts[1])
  
  # Prep data same as original function
  player <- data %>%
    filter(batter_name == player_name) %>%
    mutate(row_id = row_number())
  
  player_clean <- player %>%
    select(row_id, attack_direction, swing_path_tilt, attack_angle, 
           bat_speed, swing_length, intercept_ball_minus_batter_pos_y_inches) %>%
    filter(if_all(-row_id, ~ !is.na(.) & is.finite(.)))
  
  # PCA
  pca_model <- prcomp(player_clean %>% select(-row_id, -attack_direction), scale. = TRUE)
  pc_scores <- as.data.frame(pca_model$x)
  
  # --- Cluster on original features ---
  mclust_original <- Mclust(player_clean %>% select(-row_id, -attack_direction))
  
  # --- Cluster on PC1+PC2 ---
  mclust_4pc <- Mclust(pc_scores %>% select(PC1, PC2, PC3, PC4))
  
  # --- Cluster on PC1+PC2+PC3 ---
  mclust_3pc <- Mclust(pc_scores %>% select(PC1, PC2, PC3))
  
  # Combine for comparison
  cluster_compare <- player_clean %>%
    mutate(
      cluster_original = as.factor(mclust_original$classification),
      cluster_4pc      = as.factor(mclust_4pc$classification),
      cluster_3pc      = as.factor(mclust_3pc$classification),
      PC1 = pc_scores$PC1,
      PC2 = pc_scores$PC2,
      PC3 = pc_scores$PC3
    )
  
  # Join all three cluster assignments back to full player data via row_id
  player <- left_join(player, 
                      cluster_compare %>% select(row_id, cluster_original, cluster_4pc, cluster_3pc), 
                      by = "row_id") %>%
    mutate(
      strike_group = ifelse(strikes == 2, "2 Strikes", "<2 Strikes")
    )
  
  # --- BIC comparison (higher = better fit) ---
  cat("=== BIC Comparison (higher is better) ===\n")
  cat(sprintf("  Original features : %.1f  (%d clusters)\n", 
              mclust_original$bic, mclust_original$G))
  cat(sprintf("  PC1+PC2+PC3+PC4           : %.1f  (%d clusters)\n", 
              mclust_4pc$bic,      mclust_4pc$G))
  cat(sprintf("  PC1+PC2+PC3       : %.1f  (%d clusters)\n", 
              mclust_3pc$bic,      mclust_3pc$G))
  
  # --- Agreement between solutions (Adjusted Rand Index) ---
  cat("\n=== Cluster Agreement (ARI: 1 = identical, 0 = random) ===\n")
  cat(sprintf("  Original vs 4PC : %.3f\n", 
              adjustedRandIndex(mclust_original$classification, mclust_4pc$classification)))
  cat(sprintf("  Original vs 3PC : %.3f\n", 
              adjustedRandIndex(mclust_original$classification, mclust_3pc$classification)))
  cat(sprintf("  4PC vs 3PC      : %.3f\n", 
              adjustedRandIndex(mclust_4pc$classification, mclust_3pc$classification)))
  
  
  
  
  # --- Helper: contact quality summary for any cluster column ---
  make_contact_summary <- function(cluster_col, label) {
    result <- player %>%
      filter(!is.na(.data[[cluster_col]])) %>%
      group_by(cluster = .data[[cluster_col]]) %>%
      summarise(
        n                  = n(),
        squared_up_pct     = mean(squared_up == TRUE, na.rm = TRUE),
        blast_pct          = mean(blast == TRUE, na.rm = TRUE),
        contact_pct        = mean(!(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE),
        two_strike_contact_pct = mean(strikes == 2 & !(description %in% c("swinging_strike", "swinging_strike_blocked")), na.rm = TRUE) /
          mean(strikes == 2, na.rm = TRUE),
        xwoba              = mean(expected_woba, na.rm = TRUE),
        swing_length       = mean(swing_length, na.rm = TRUE),
        swing_path_tilt    = mean(swing_path_tilt, na.rm = TRUE),
        attack_angle       = mean(attack_angle, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(solution = label)
    
    cat(paste0("\n=== Contact Quality by Cluster: ", label, " ===\n"))
    print(result)
    return(result)
  }
  
  contact_original <- make_contact_summary("cluster_original", "Original Features")
  contact_4pc      <- make_contact_summary("cluster_4pc",      "PC1 + PC2")
  contact_3pc      <- make_contact_summary("cluster_3pc",      "PC1 + PC2 + PC3")
  
  # Stack all three for easy comparison
  contact_all <- bind_rows(contact_original, contact_4pc, contact_3pc)
  # --- Helper to build plot8 for any cluster assignment ---
  make_strike_plot <- function(cluster_col, label) {
    ggplot(player %>% filter(!is.na(.data[[cluster_col]]), !is.na(strikes))) +
      geom_bar(aes(x = strike_group, fill = .data[[cluster_col]]), position = "fill") +
      ggtitle(paste0(name_title, " - Cluster vs. Strike Count Group\n(", label, ")")) +
      xlab("Strike Group") +
      ylab("Proportion") +
      labs(fill = "Cluster") +
      theme_minimal() +
      theme(
        plot.title   = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x  = element_text(size = 11),
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 11)
      )
  }
  
  plot8_original <- make_strike_plot("cluster_original", "Original Features")
  plot8_4pc      <- make_strike_plot("cluster_4pc",      "PC1 + PC2 + PC3 + PC4")
  plot8_3pc      <- make_strike_plot("cluster_3pc",      "PC1 + PC2 + PC3")
  
  # --- PCA scatter plots colored by each clustering ---
  make_pca_plot <- function(cluster_col, label) {
    ggplot(cluster_compare, aes(x = PC1, y = PC2, color = .data[[cluster_col]])) +
      geom_point(alpha = 0.6, size = 2) +
      ggtitle(paste0(name_title, " - ", label)) +
      theme_minimal() + 
      scale_color_brewer(palette = "Set1") +
      labs(color = "Cluster")
  }
  
  p_original <- make_pca_plot("cluster_original", "Original Features Clustering")
  p_4pc      <- make_pca_plot("cluster_4pc",      "2-PC Clustering")
  p_3pc      <- make_pca_plot("cluster_3pc",      "3-PC Clustering")
  
  pca_combined <- p_original + p_4pc + p_3pc +
    plot_layout(ncol = 3) +
    plot_annotation(title = paste(name_title, "- PCA Scatter Comparison"))
  
  strike_combined <- plot8_original + plot8_4pc + plot8_3pc +
    plot_layout(ncol = 3) +
    plot_annotation(title = paste(name_title, "- Strike Count Group Comparison"))
  
  print(pca_combined)
  print(strike_combined)
  
  return(list(
    cluster_compare  = cluster_compare,
    player           = player,
    mclust_original  = mclust_original,
    mclust_4pc       = mclust_4pc,
    mclust_3pc       = mclust_3pc,
    pca_combined     = pca_combined,
    strike_combined  = strike_combined,
    plot8_original   = plot8_original,
    plot8_4pc        = plot8_4pc,
    plot8_3pc        = plot8_3pc,
    contact_original = contact_original,
    contact_4pc      = contact_4pc,
    contact_3pc      = contact_3pc,
    contact_all      = contact_all
  ))
}

# Run it


pca_cluster_comparison <- compare_pca_clusters(swing_data_2025, "Altuve, Jose")

pca_cluster_comparison$contact_original

pca_cluster_comparison$contact_3pc

pca_cluster_comparison$plot8_3pc


pca_cluster_comparison$cluster_compare

player=swing_data_2025 %>% filter(batter_name=="Muncy, Max")

player <- player %>%
  mutate(
    intercept_zone = case_when(
      intercept_ball_minus_batter_pos_y_inches < quantile(intercept_ball_minus_batter_pos_y_inches, 0.33, na.rm = TRUE) ~ "Early (Out Front)",
      intercept_ball_minus_batter_pos_y_inches > quantile(intercept_ball_minus_batter_pos_y_inches, 0.67, na.rm = TRUE) ~ "Late (Deep)",
      TRUE ~ "Middle"
    )
  )

# 1. Density plot - attack direction distribution by intercept zone
# Best for seeing how the full distribution shifts
p1 <- ggplot(player, aes(x = attack_direction, fill = intercept_zone, color = intercept_zone)) +
  geom_density(alpha = 0.3) +
  ggtitle("Attack Direction Distribution by Intercept Zone") +
  xlab("Attack Direction") + ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# 2. Scatter with smoothed trend - shows raw relationship + trend
# Best for seeing the continuous relationship
p2 <- ggplot(player, aes(x = intercept_ball_minus_batter_pos_y_inches, y = attack_direction)) +
  geom_point(alpha = 0.2, size = 1.5) +
  geom_smooth(method = "loess", color = "firebrick", se = TRUE) +
  ggtitle("Attack Direction by Intercept Point") +
  xlab("Intercept Point (inches)") + ylab("Attack Direction") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))


# 4. 2D density / heatmap - shows where observations concentrate
# Best for seeing joint distribution hotspots
p4 <- ggplot(player, aes(x = intercept_ball_minus_batter_pos_y_inches, y = attack_direction)) +
  geom_bin2d(bins = 40) +
  scale_fill_viridis_c(option = "magma") +
  geom_smooth(method = "loess", color = "white", se = FALSE, linewidth = 1) +
  ggtitle("Attack Direction vs Intercept Point - Joint Density") +
  xlab("Intercept Point (inches)") + ylab("Attack Direction") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Print all together

p1

p2

p4

player=swing_data_2025 %>% filter(batter_name=="Goldschmidt, Paul")

player <- player %>%
  mutate(
    intercept_zone = case_when(
      intercept_ball_minus_batter_pos_y_inches < quantile(intercept_ball_minus_batter_pos_y_inches, 0.33, na.rm = TRUE) ~ "Early (Out Front)",
      intercept_ball_minus_batter_pos_y_inches > quantile(intercept_ball_minus_batter_pos_y_inches, 0.67, na.rm = TRUE) ~ "Late (Deep)",
      TRUE ~ "Middle"
    )
  )

# 1. Density plot - attack direction distribution by intercept zone
# Best for seeing how the full distribution shifts
p1 <- ggplot(player, aes(x = attack_direction, fill = intercept_zone, color = intercept_zone)) +
  geom_density(alpha = 0.3) +
  ggtitle("Attack Direction Distribution by Intercept Zone") +
  xlab("Attack Direction") + ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# 2. Scatter with smoothed trend - shows raw relationship + trend
# Best for seeing the continuous relationship
p2 <- ggplot(player, aes(x = intercept_ball_minus_batter_pos_y_inches, y = attack_direction)) +
  geom_point(alpha = 0.2, size = 1.5) +
  geom_smooth(method = "loess", color = "firebrick", se = TRUE) +
  ggtitle("Attack Direction by Intercept Point") +
  xlab("Intercept Point (inches)") + ylab("Attack Direction") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))


# 4. 2D density / heatmap - shows where observations concentrate
# Best for seeing joint distribution hotspots
p4 <- ggplot(player, aes(x = intercept_ball_minus_batter_pos_y_inches, y = attack_direction)) +
  geom_bin2d(bins = 40) +
  scale_fill_viridis_c(option = "magma") +
  geom_smooth(method = "loess", color = "white", se = FALSE, linewidth = 1) +
  ggtitle("Attack Direction vs Intercept Point - Joint Density") +
  xlab("Intercept Point (inches)") + ylab("Attack Direction") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Print all together

p1

p2

p4






player=swing_data_2025 %>% filter(batter_name=="Ohtani, Shohei")

player <- player %>%
  mutate(
    intercept_zone = case_when(
      intercept_ball_minus_batter_pos_y_inches < quantile(intercept_ball_minus_batter_pos_y_inches, 0.33, na.rm = TRUE) ~ "Early (Out Front)",
      intercept_ball_minus_batter_pos_y_inches > quantile(intercept_ball_minus_batter_pos_y_inches, 0.67, na.rm = TRUE) ~ "Late (Deep)",
      TRUE ~ "Middle"
    )
  )

# 1. Density plot - attack direction distribution by intercept zone
# Best for seeing how the full distribution shifts
p1 <- ggplot(player, aes(x = attack_direction, fill = intercept_zone, color = intercept_zone)) +
  geom_density(alpha = 0.3) +
  ggtitle("Attack Direction Distribution by Intercept Zone") +
  xlab("Attack Direction") + ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))

p1


player=swing_data_2025 %>% filter(batter_name=="Goldschmidt, Paul")

player <- player %>%
  mutate(
    intercept_zone = case_when(
      intercept_ball_minus_batter_pos_y_inches < quantile(intercept_ball_minus_batter_pos_y_inches, 0.33, na.rm = TRUE) ~ "Early (Out Front)",
      intercept_ball_minus_batter_pos_y_inches > quantile(intercept_ball_minus_batter_pos_y_inches, 0.67, na.rm = TRUE) ~ "Late (Deep)",
      TRUE ~ "Middle"
    )
  )

# 1. Density plot - attack direction distribution by intercept zone
# Best for seeing how the full distribution shifts
p1 <- ggplot(player, aes(x = attack_direction, fill = intercept_zone, color = intercept_zone)) +
  geom_density(alpha = 0.3) +
  ggtitle("Attack Direction Distribution by Intercept Zone") +
  xlab("Attack Direction") + ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))
p1


early_cut  <- quantile(swing_data_2025$intercept_ball_minus_batter_pos_y_inches, 0.33, na.rm = TRUE)
late_cut   <- quantile(swing_data_2025$intercept_ball_minus_batter_pos_y_inches, 0.67, na.rm = TRUE)

player <- swing_data_2025 %>%
  filter(batter_name == "Muncy, Max") %>%
  mutate(
    intercept_zone = case_when(
      intercept_ball_minus_batter_pos_y_inches < early_cut ~ "Early (Out Front)",
      intercept_ball_minus_batter_pos_y_inches > late_cut  ~ "Late (Deep)",
      TRUE ~ "Middle"
    )
  )


p1 <- ggplot(player, aes(x = attack_direction, fill = intercept_zone, color = intercept_zone)) +
  geom_density(alpha = 0.3) +
  ggtitle("Attack Direction Distribution by Intercept Zone") +
  xlab("Attack Direction") + ylab("Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"))
p1


library(tidyverse)
library(ranger)
library(vip)
set.seed(42)


# Read the clustered combined data
combined_data <- read_csv("savant_data_with_clusters_2024_2025.csv")

# ---- Add count column ----
combined_data <- combined_data %>%
  mutate(count = paste0(balls, "-", strikes),
         count = as.factor(count))

# ---- Add lag/sequence features ----
combined_data <- combined_data %>%
  arrange(game_id, at_bat_number, pitch_number) %>%
  group_by(game_id, at_bat_number) %>%
  mutate(
    prev_pitch_name = lag(pitch_name),
    prev_cluster    = lag(cluster),
    prev_swing      = lag(swing),
    prev_zone       = lag(zone)
  ) %>%
  ungroup()

# ---- Clean NAs in lag features ----
combined_data <- combined_data %>%
  mutate(
    prev_pitch_name = ifelse(is.na(prev_pitch_name), "None", as.character(prev_pitch_name)),
    prev_pitch_name = as.factor(prev_pitch_name),
    prev_cluster    = ifelse(is.na(prev_cluster), "None", as.character(prev_cluster)),
    prev_cluster    = as.factor(prev_cluster),
    prev_swing      = ifelse(is.na(prev_swing), 0, prev_swing),
    prev_zone       = as.factor(prev_zone),
    zone            = as.factor(zone),
    prev_zone       = ifelse(is.na(prev_zone), 0, prev_zone),
    cluster         = as.factor(cluster)
  )

# ---- Collapse pitch types for RF ----
combined_data <- combined_data %>%
  mutate(
    pitch_name = fct_lump_min(as.character(pitch_name), min = 5000),
    pitch_name = case_when(
      pitch_name %in% c("4-Seam Fastball", "Sinker")                                  ~ "Fastball",
      pitch_name %in% c("Changeup", "Split-Finger")                                   ~ "Offspeed",
      pitch_name %in% c("Curveball", "Knuckle Curve", "Slider", "Slurve", "Sweeper") ~ "Breaking",
      TRUE ~ as.character(pitch_name)
    ),
    pitch_name = as.factor(pitch_name)
  )

# ---- Train pitch type RF and add pred column ----
set.seed(42)
pitch_rf <- ranger(
  pitch_name ~ outs + inning + bat_side + pitch_hand +
    release_pos_x + release_pos_z + release_pos_y +
    extension + bat_score_diff + count + prev_pitch_name,
  num.trees = 500,
  importance = "impurity",
  data = combined_data %>% filter(!is.na(pitch_name))
)

combined_data <- combined_data %>%
  mutate(pred = pitch_rf$predictions)

# ---- Write final file ----
write_csv(combined_data, "savant_data_with_clusters_and_preds222.csv")


pitch_rf
vip(pitch_rf)


prop.table(table(combined_data$pitch_name))

pitch_rf$variable.importance

data %>% colnames()

combined_data %>% summarize(mean(pitch_name==pred))


conf_by_count <- data %>%
  group_by(count) %>%
  summarise(
    confusion = list(table(True = pitch_name,
                           Predicted = pred)),
    accuracy = mean(pitch_name == pred)
  )

conf_by_count$confusion[[which(conf_by_count$count == "0-0")]]

data %>% distinct(pitch_name)

conf_by_count %>% arrange(accuracy)

accuracy_by_prev <- data %>%
  group_by(prev_pitch_name) %>%
  summarise(
    n = n(),
    accuracy = mean(pitch_name == pred)
  ) %>%
  arrange(accuracy) %>% filter(prev_pitch_name %in% c("Cutter", "Curveball",
                                                      "Changeup", "4-Seam Fastball", "None", "Slider"))

accuracy_by_prev

conf_by_count %>% arrange(desc(accuracy)) %>% 
  mutate(count = reorder(count, accuracy)) %>% 
  ggplot(aes(x = accuracy, y = count)) + 
  geom_col(fill = "steelblue") + 
  labs( title = "Pitch Type Prediction Accuracy by Count", 
        x = "Prediction Accuracy", y = "Count" ) + 
  theme_minimal()





# data %>%
#   filter(!is.na(cluster), !is.na(count), batter_name=="Volpe, Anthony", year==2025,
#          count %in% c("1-1", "2-2", "0-1", "2-0", "3-0", "3-1")) %>%
#   mutate(cluster = factor(cluster)) %>%
#   ggplot(aes(x = count, fill = cluster)) +
#   geom_bar(position = "fill") +
#   labs(
#     title = "Swing Quality Distribution by Count",
#     x = "Count",
#     y = "Proportion",
#     fill = "Cluster"
#   ) +
#   scale_y_continuous(labels = scales::percent_format()) +
#   theme_minimal()



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
    geom_bar(aes(x = strikes, fill = cluster), position = "fill") +
    ggtitle(paste0(name_title, " - Cluster vs. Strikes")) +
    xlab("Strikes") +
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
      problem_players_2025 <<- c(problem_players_2025, player)
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


min_swings <- 500

hitter_counts <- data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  summarize(n_swings = n(), .groups = "drop") %>%
  filter(n_swings >= min_swings)

# Remove n_swings if it already exists in data from a previous run
data <- data %>% select(-any_of("n_swings"))

filtered_data <- data %>%
  inner_join(hitter_counts, by = c("batter_name", "year"))

# Verify
filtered_data %>%
  distinct(batter_name, year, n_swings) %>%
  arrange(n_swings)







run_seq_test <- function(df) {
  tryCatch({
    df$cluster <- droplevels(df$cluster)
    
    # Need enough data and cluster variety
    if (nrow(df) < 100 || length(levels(df$cluster)) < 2) return(tibble())
    
    model_seq <- ranger(
      cluster ~ pitch_hand + count + outs +
        prev_pitch_name + prev_swing + prev_zone +
        pitch_name + zone,
      num.trees = 500,
      importance = "impurity",
      probability = FALSE,
      data = df
    )
    
    model_no_seq <- ranger(
      cluster ~ pitch_hand + pitch_name + zone,
      num.trees = 500,
      importance = "impurity",
      probability = FALSE,
      keep.inbag = TRUE,
      data = df
    )
    
    # OOB predictions - honest out-of-sample accuracy
    acc_seq    <- mean(model_seq$predictions    == df$cluster, na.rm = TRUE)
    acc_no_seq <- mean(model_no_seq$predictions == df$cluster, na.rm = TRUE)
    
    tibble(
      acc_seq    = acc_seq,
      acc_no_seq = acc_no_seq,
      gain       = acc_seq - acc_no_seq,
      n          = nrow(df),
      n_clusters = length(levels(df$cluster))
    )
    
  }, error = function(e) tibble())
}





results <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  group_modify(~ run_seq_test(.x)) %>%
  ungroup()

# After getting results, look at it this way:
results %>%
  summarize(
    mean_oob_seq    = mean(acc_seq, na.rm = TRUE),
    mean_oob_no_seq = mean(acc_no_seq, na.rm = TRUE),
    mean_gain       = mean(gain, na.rm = TRUE),
    pct_positive    = mean(gain > 0, na.rm = TRUE)
  )


run_seq_test_v2 <- function(df) {
  tryCatch({
    df$cluster <- droplevels(df$cluster)
    if (nrow(df) < 100 || length(levels(df$cluster)) < 2) return(tibble())
    
    model_seq <- ranger(
      cluster ~ pitch_hand + count + outs +
        prev_pitch_name + prev_swing + prev_zone +
        pitch_name + zone,
      num.trees = 500,
      importance = "impurity",
      data = df
    )
    
    model_no_seq <- ranger(
      cluster ~ pitch_hand + pitch_name + zone,
      num.trees = 500,
      importance = "impurity",
      data = df
    )
    
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
      by_cluster = cluster_gains,
      importance = seq_importance
    )
    
  }, error = function(e) NULL)
}

importance_results <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  group_modify(~ {
    res <- run_seq_test_v2(.x)
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

lm_fit <- lm(gain ~ avg_seq_importance, data = merged)
summary(lm_fit)

r2 <- summary(lm_fit)$r.squared
r2

correlation <- cor(merged$avg_seq_importance,
                   merged$gain,
                   use = "complete.obs")

correlation

ggplot(merged, aes(x = avg_seq_importance, y = gain)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Sequencing Importance vs Predictive Gain",
    subtitle = paste0("R² = ", round(r2, 3)),
    x = "Average Sequencing Variable Importance",
    y = "Sequencing Accuracy Gain"
  ) +
  theme_minimal(base_size = 14)

all_results <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  group_modify(~ {
    res <- run_seq_test_v2(.x)
    if (is.null(res)) return(tibble())
    res$overall
  }) %>%
  ungroup()


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

# Which clusters gain most from sequencing info across hitters?
cluster_summary <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  group_modify(~ {
    res <- run_seq_test_v2(.x)
    if (is.null(res)) return(tibble())
    res$by_cluster
  }) %>%
  ungroup() %>%
  group_by(batter_name, year, cluster) %>%
  summarize(
    mean_gain = mean(cluster_gain, na.rm = TRUE),
    mean_n    = mean(cluster_n, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_gain))

print(cluster_summary)

centroids <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  mutate(contact= !(description %in% c("swinging_strike", "swinging_strike_blocked"))) %>% 
  group_by(batter_name, year, cluster) %>%
  summarize(n=n(),
            across(c(bat_speed, swing_length, attack_angle, swing_path_tilt, intercept_ball_minus_batter_pos_y_inches,
                     squared_up, expected_woba, launch_speed, contact), # replace with your actual gmm input vars
                   mean, na.rm = TRUE)
  )

total_gain = cluster_summary %>% group_by(batter_name, year) %>% 
  summarize(total=sum(mean_gain)) %>% ungroup()

total_gain %>% summarize(sum(total>0))


contreras=centroids %>% filter(batter_name==("Contreras, Willson")) 

lm_fit <- lm(gain ~ avg_seq_importance, data = merged)
summary(lm_fit)
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

library(gt)
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




data_2025 %>% colnames()
data %>% colnames()
diaz %>% colnames()



