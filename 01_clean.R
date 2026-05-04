library(sabRmetrics)
library(tidyverse)

# ---- Helper functions ----

recreate_squared_up <- function(data) {
  data |>
    dplyr::mutate(
      plate_y    = 17 / 12,
      plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
      plate_speed = 0.6818182 * sqrt(
        (ax * plate_time + bx)^2 + (ay * plate_time + by)^2 + (az * plate_time + bz)^2
      ),
      squared_up = ifelse(
        test = description == "hit_into_play" & !is.na(launch_speed),
        yes  = (launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) > 0.8,
        no   = FALSE
      )
    ) |>
    dplyr::select(dplyr::all_of(colnames(data)), squared_up)
}

recreate_blasts <- function(data) {
  data |>
    dplyr::mutate(
      plate_y    = 17 / 12,
      plate_time = (-by - sqrt(by^2 - 4 * (ay / 2) * (cy - plate_y))) / (2 * (ay / 2)),
      plate_speed = 0.6818182 * sqrt(
        (ax * plate_time + bx)^2 + (ay * plate_time + by)^2 + (az * plate_time + bz)^2
      ),
      blast = ifelse(
        test = description == "hit_into_play" & !is.na(launch_speed),
        yes  = ((launch_speed / (1.23 * bat_speed + 0.23 * plate_speed)) * 100) + bat_speed >= 164,
        no   = FALSE
      )
    ) |>
    dplyr::select(dplyr::all_of(colnames(data)), blast)
}

clean_data <- function(data) {
  data <- data |>
    sabRmetrics::get_quadratic_coef(source = "baseballsavant") |>
    sabRmetrics::get_trackman_metrics()
  
  data <- recreate_squared_up(data)
  data <- recreate_blasts(data)
  
  data <- data %>%
    mutate(
      outcome = case_when(
        events %in% c("fielders_choice", "field_out", "fielders_choice_out",
                      "force_out", "double_play", "triple_play", "strikeout",
                      "grounded_into_double_play", "field_error",
                      "strikeout_double_play")                          ~ "out",
        events %in% c("single", "home_run", "double", "triple")        ~ "hit",
        events %in% c("walk", "hit_by_pitch", "intent_walk")           ~ "walk",
        events %in% c("sac_fly", "sac_fly_double_play", "sac_bunt")    ~ "sacrifice",
        TRUE                                                            ~ "other"
      ),
      middle      = (strike_zone_bottom + strike_zone_top) / 2,
      swing       = description %in% c("foul", "hit_into_play", "swinging_strike",
                                       "swinging_strike_blocked", "foul_tip"),
      ideal_attack_angle = as_factor(ifelse(attack_angle >= 5 & attack_angle <= 20, 1, 0)),
      runner_on_first  = !is.na(pre_runner_1b_id),
      runner_on_second = !is.na(pre_runner_2b_id),
      runner_on_third  = !is.na(pre_runner_3b_id),
      base_state = case_when(
        runner_on_first & runner_on_second & runner_on_third ~ "1B_2B_3B",
        runner_on_first & runner_on_second                   ~ "1B_2B",
        runner_on_first & runner_on_third                    ~ "1B_3B",
        runner_on_second & runner_on_third                   ~ "2B_3B",
        runner_on_first                                      ~ "1B",
        runner_on_second                                     ~ "2B",
        runner_on_third                                      ~ "3B",
        TRUE                                                 ~ "Empty"
      ),
      game_state = paste0(base_state, " | ", outs, " Outs")
    )
  
  return(data)
}

# ---- Run ----

data_2024 <- read_csv("data/savant_data_2024.csv") |> clean_data()
data_2025 <- read_csv("data/savant_data_2025.csv") |> clean_data()

write_csv(data_2024, "data/savant_data_clean_2024.csv")
write_csv(data_2025, "data/savant_data_clean_2025.csv")