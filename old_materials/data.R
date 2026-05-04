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

data_baseballsavant_2024=data_baseballsavant_2025 %>% mutate(middle=(strike_zone_bottom+strike_zone_top)/2) 

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


swing_data_2025 <- swing_data_2024 %>%
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





