library(mclust)
library(ggplot2)
library(dplyr)
library(ggmosaic)
library(factoextra)
library(stringr)
library(tidyverse)
theme_set(theme_light())

swing_data_2024 = read_csv("swing_data_2025.csv")


swing_data_2024 %>% 
  ggplot(aes(x=attack_direction, y=intercept_ball_minus_batter_pos_y_inches))+
  geom_point()


judge = swing_data_2024 %>% filter(batter_name=="Judge, Aaron")

judge %>% 
  ggplot(aes(x=attack_direction, y=intercept_ball_minus_batter_pos_y_inches, 
             colour = blast, alpha = 0.7))+
  geom_point()

judge %>% 
  ggplot(aes(x=bat_speed, y=swing_length, 
             colour = blast, alpha = 0.7))+
  geom_point()

arraez = swing_data_2024 %>% filter(batter_name=="Arraez, Luis")

arraez %>% 
  ggplot(aes(x=attack_direction, y=intercept_ball_minus_batter_pos_y_inches, 
             colour = blast, alpha = 0.7))+
  geom_point()

arraez %>% 
  ggplot(aes(x=bat_speed, y=swing_length, 
             colour = blast, alpha = 0.7))+
  geom_point()

ohtani = swing_data_2024 %>% filter(batter_name=="Ohtani, Shohei", attack_direction<100)

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
    geom_mosaic(aes(x = product(cluster, leverage_group), fill = cluster)) +
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
    geom_mosaic(data=player,
                aes(x = product(cluster, runner_scoring_position), fill = cluster)) +
    ggtitle(paste(name_title, "- Cluster vs. RSP"))
  
  plot2 <- ggplot(player) +
    geom_mosaic(aes(x = product(cluster, sac_fly_op), fill = cluster)) +
    ggtitle(paste(name_title, "- Cluster vs. Sac Fly Opportunity"))
  
  plot3 <- ggplot(player) +
    geom_mosaic(aes(x = product(cluster, strikes), fill = cluster)) +
    ggtitle(paste(name_title, "- Cluster vs. Strikes"))
  
  plot5 <- ggplot(player) +
    geom_mosaic(aes(x = product(cluster, late_sac_fly_op), fill = cluster)) +
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
    geom_mosaic(aes(x = product(cluster, pitch_name), fill = cluster)) +
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
    geom_mosaic(aes(x = product(cluster, strike_group), fill = cluster)) +
    labs(title = paste0(name_title, " - Cluster vs. Strike Count Group"),
         x = "Cluster × Strike Group",
         y = "Proportion") +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 18, face = "bold"),  # Title size
      axis.title.x = element_text(size = 14),               # X-axis label size
      axis.title.y = element_text(size = 14),               # Y-axis label size
      axis.text.x = element_text(size = 12),
      legend.title = element_text(size = 14),     # Legend title size
      legend.text = element_text(size = 12),
      axis.text.y = element_blank()
    )
  
  # Mosaic plot: Location Zone vs Cluster (2-strike counts only)
  plot7 <- player %>%
    filter(strikes == 2) %>%
    ggplot() +
    geom_mosaic(aes(x = product(cluster, location_zone), fill = cluster)) +
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


plots <- plot_player_clusters(swing_data_2024, "Altuve, Jose")



plots$rsp_plot
plots$sac_fly_plot
plots$strikes_plot
plots$launch_plot
plots$plot_leverage
plots$avg_launch_df
x=plots$contact_by_cluster
plots$pitch_type_2s_plot
plots$location_2s_plot
plots$plot8



library(dplyr)
library(ggplot2)
library(scales)
library(ggalluvial)

set.seed(42)

toy_data <- tibble(
  cluster = sample(c("1", "2"), 600, replace = TRUE, prob = c(.6, .4)),
  runner_scoring_position = sample(c(TRUE, FALSE), 600, replace = TRUE),
  sac_fly_op = sample(c(TRUE, FALSE), 600, replace = TRUE),
  strikes = sample(0:2, 600, replace = TRUE),
  pitch_name = sample(c("Fastball", "Slider", "Changeup"), 600, replace = TRUE),
  location_zone = sample(
    c("high-inside", "high-middle", "high-outside",
      "middle-inside", "middle-middle", "middle-outside",
      "low-inside", "low-middle", "low-outside"),
    600, replace = TRUE
  )
)

toy_data %>%
  count(cluster, runner_scoring_position) %>%
  group_by(cluster) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = cluster, y = pct, fill = runner_scoring_position)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Cluster vs RSP (Stacked Bar Replacement)",
    y = "Proportion"
  ) +
  theme_minimal()
