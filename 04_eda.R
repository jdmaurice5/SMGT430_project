library(tidyverse)
library(mclust)
library(ggplot2)
library(factoextra)
library(stringr)
library(patchwork)

theme_set(theme_light())

swing_data_2025 <- read_csv("data/savant_data_clean_2025.csv") %>%
  filter(swing == TRUE)

# ---- League-wide scatter ----
swing_data_2025 %>%
  ggplot(aes(x = attack_direction, y = intercept_ball_minus_batter_pos_y_inches)) +
  geom_point()

# ---- Individual player EDA ----

plot_intercept_eda <- function(data, player_name) {
  player <- data %>%
    filter(batter_name == player_name) %>%
    mutate(
      intercept_zone = case_when(
        intercept_ball_minus_batter_pos_y_inches < quantile(intercept_ball_minus_batter_pos_y_inches, 0.33, na.rm = TRUE) ~ "Early (Out Front)",
        intercept_ball_minus_batter_pos_y_inches > quantile(intercept_ball_minus_batter_pos_y_inches, 0.67, na.rm = TRUE) ~ "Late (Deep)",
        TRUE ~ "Middle"
      )
    )
  
  p1 <- ggplot(player, aes(x = attack_direction, fill = intercept_zone, color = intercept_zone)) +
    geom_density(alpha = 0.3) +
    ggtitle(paste(player_name, "- Attack Direction by Intercept Zone")) +
    xlab("Attack Direction") + ylab("Density") +
    theme_minimal()
  
  p2 <- ggplot(player, aes(x = intercept_ball_minus_batter_pos_y_inches, y = attack_direction)) +
    geom_point(alpha = 0.2, size = 1.5) +
    geom_smooth(method = "loess", color = "firebrick", se = TRUE) +
    ggtitle(paste(player_name, "- Attack Direction by Intercept Point")) +
    xlab("Intercept Point (inches)") + ylab("Attack Direction") +
    theme_minimal()
  
  p4 <- ggplot(player, aes(x = intercept_ball_minus_batter_pos_y_inches, y = attack_direction)) +
    geom_bin2d(bins = 40) +
    scale_fill_viridis_c(option = "magma") +
    geom_smooth(method = "loess", color = "white", se = FALSE, linewidth = 1) +
    ggtitle(paste(player_name, "- Attack Direction vs Intercept Point")) +
    xlab("Intercept Point (inches)") + ylab("Attack Direction") +
    theme_minimal()
  
  print(p1); print(p2); print(p4)
}

for (p in c("Judge, Aaron", "Arraez, Luis", "Ohtani, Shohei",
            "Muncy, Max", "Goldschmidt, Paul")) {
  plot_intercept_eda(swing_data_2025, p)
}

# ---- Blast/bat speed scatter for key players ----

for (p in c("Judge, Aaron", "Arraez, Luis", "Ohtani, Shohei")) {
  player <- swing_data_2025 %>% filter(batter_name == p)
  print(
    player %>%
      ggplot(aes(x = bat_speed, y = swing_length, colour = blast, alpha = 0.7)) +
      geom_point() + ggtitle(paste(p, "- Bat Speed vs Swing Length"))
  )
}