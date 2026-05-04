library(tidyverse)
library(ranger)
library(vip)
set.seed(42)


combined_data = read_csv("savant_data_with_clusters_2024_2025.csv")

combined_data <- combined_data %>% filter(!is.na(pitch_name)) %>% 
  arrange(game_id, at_bat_number, pitch_number) %>%
  group_by(game_id, at_bat_number) %>%
  mutate(prev_pitch_name = lag(pitch_name)) %>%
  ungroup()

combined_data <- combined_data %>%
  mutate(
    pitch_name = fct_lump_min(pitch_name, min = 5000)
  )

combined_data = combined_data %>% mutate(pitch_name=case_when(
  pitch_name %in% c("4-Seam Fastball", "Sinker") ~ "Fastball",
  pitch_name %in% c("Changeup", "Split-Finger") ~ "Offspeed",
  pitch_name %in% c("Curveball", "Knuckle Curve", "Slider", "Slurve", "Sweeper") ~ "Breaking",
  TRUE ~ pitch_name
))

combined_data = combined_data %>% mutate(prev_pitch_type=case_when(
  prev_pitch_name %in% c("4-Seam Fastball", "Sinker") ~ "Fastball",
  prev_pitch_name %in% c("Changeup", "Split-Finger") ~ "Offspeed",
  prev_pitch_name %in% c("Curveball", "Knuckle Curve", "Slider", "Slurve", "Sweeper") ~ "Breaking",
  TRUE ~ prev_pitch_name
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
  mutate(count = paste0(balls, "-", strikes),
         count=as.factor(count))


# pitch_rf <- ranger(pitch_name ~ outs + inning + bat_side + pitch_hand+
#                      balls+strikes+release_pos_x+release_pos_z+release_pos_y+
#                      extension+bat_score_diff+strikes+balls+prev_pitch_name, 
#                    num.trees = 500, importance = "impurity", data = combined_data)

pitch_rf <- ranger(pitch_name ~ outs + inning + bat_side + pitch_hand+
                     release_pos_x+release_pos_z+release_pos_y+
                     extension+bat_score_diff+count+prev_pitch_name, 
                   num.trees = 500, importance = "impurity", data = combined_data)



pitch_rf
vip(pitch_rf)
combined_data=combined_data |> 
  mutate(pred = pitch_rf$predictions) 


prop.table(table(combined_data$pitch_name))

pitch_rf$variable.importance

data %>% colnames()

combined_data %>% summarize(mean(pitch_name==pred))

write.csv(combined_data, "savant_data_with_pitch_predictions.csv")

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



