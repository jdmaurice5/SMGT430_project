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













