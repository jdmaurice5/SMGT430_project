library(tidyverse)
library(ranger)
library(vip)

set.seed(42)

combined_data <- read_csv("data/savant_data_with_clusters_2024_2025.csv")

combined_data <- combined_data %>% filter(!is.na(pitch_name)) %>% 
  arrange(game_id, at_bat_number, pitch_number) %>%
  group_by(game_id, at_bat_number) %>%
  mutate(prev_pitch_name = lag(pitch_name),
         prev_cluster    = lag(cluster),
         prev_swing      = lag(swing),
         prev_zone       = lag(zone)) %>%
  ungroup()

combined_data <- combined_data %>%
  mutate(
    pitch_name = fct_lump_min(pitch_name, min = 5000)
  )

combined_data = combined_data %>% mutate(pitch_name=case_when(
  pitch_name %in% c("4-Seam Fastball", "Sinker") ~ "Fastball",
  pitch_name %in% c("Changeup", "Split-Finger") ~ "Offspeed",
  pitch_name %in% c("Curveball", "Knuckle Curve", "Slider", "Slurve", "Sweeper") ~ "Breaking",
  pitch_name %in% c("Cutter") ~ "Cutter",
  TRUE ~ "Other"
))

combined_data = combined_data %>% mutate(prev_pitch_type=case_when(
  prev_pitch_name %in% c("4-Seam Fastball", "Sinker") ~ "Fastball",
  prev_pitch_name %in% c("Changeup", "Split-Finger") ~ "Offspeed",
  prev_pitch_name %in% c("Curveball", "Knuckle Curve", "Slider", "Slurve", "Sweeper") ~ "Breaking",
  pitch_name %in% c("Cutter") ~ "Cutter",
  TRUE ~ "Other"
))

combined_data <- combined_data %>%
  mutate(
    pitch_name = as.factor(pitch_name)
  )

combined_data <- combined_data %>%
  mutate(
    prev_pitch_name = ifelse(is.na(prev_pitch_name),
                             "None",
                             as.character(prev_pitch_name)),
    prev_pitch_name = as.factor(prev_pitch_name)
  )

combined_data <- combined_data %>%
  mutate(
    prev_pitch_name = as.factor(ifelse(is.na(prev_pitch_name), "None", prev_pitch_name)),
    prev_cluster    = as.factor(ifelse(is.na(prev_cluster), "None", as.character(prev_cluster))),
    prev_swing      = ifelse(is.na(prev_swing), 0, prev_swing),
    prev_zone       = as.factor(ifelse(is.na(prev_zone), 0, prev_zone)),
    zone            = as.factor(zone),
    cluster         = as.factor(cluster)
  )

combined_data <- combined_data %>%
  mutate(count = paste0(balls, "-", strikes),
         count=as.factor(count))


# pitch_rf <- ranger(pitch_name ~ outs + inning + bat_side + pitch_hand+
#                      balls+strikes+release_pos_x+release_pos_z+release_pos_y+
#                      extension+bat_score_diff+strikes+balls+prev_pitch_name, 
#                    num.trees = 500, importance = "impurity", data = combined_data)


variables = combined_data %>% select(game_id, outs, inning, bat_side, pitch_hand,
                                     release_pos_x, release_pos_y, release_pos_z, extension,
                                     bat_score_diff, count, prev_pitch_name, pitch_name)

pitch_rf <- ranger(
  pitch_name ~ outs + inning + bat_side + pitch_hand +
    release_pos_x + release_pos_z + release_pos_y +
    extension + bat_score_diff + count + prev_pitch_name,
  num.trees  = 500,
  importance = "impurity",
  data       = variables %>% filter(!is.na(pitch_name))
)

combined_data <- combined_data %>%
  mutate(pred = pitch_rf$predictions)

# ---- Write final dataset ----

write_csv(combined_data, "data/savant_data_with_clusters_and_preds.csv")

# ---- Quick diagnostics ----

vip(pitch_rf)
prop.table(table(combined_data$pitch_name))
combined_data %>% summarize(accuracy = mean(pitch_name == pred, na.rm = TRUE))

conf_by_count <- combined_data %>%
  group_by(count) %>%
  summarise(
    confusion = list(table(True = pitch_name, Predicted = pred)),
    accuracy  = mean(pitch_name == pred, na.rm = TRUE)
  )

conf_by_count %>% arrange(accuracy)
conf_by_count %>% arrange(desc(accuracy)) %>%
  mutate(count = reorder(count, accuracy)) %>%
  ggplot(aes(x = accuracy, y = count)) +
  geom_col(fill = "steelblue") +
  labs(title = "Pitch Type Prediction Accuracy by Count",
       x = "Prediction Accuracy", y = "Count") +
  theme_minimal()

accuracy_by_prev <- combined_data %>%
  group_by(prev_pitch_name) %>%
  summarise(n = n(), accuracy = mean(pitch_name == pred, na.rm = TRUE)) %>%
  arrange(accuracy) 
accuracy_by_prev
