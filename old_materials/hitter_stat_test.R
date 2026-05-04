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
      keep.inbag = TRUE,
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


run_seq_test_v3 <- function(df) {
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
      overall = tibble(acc_seq, acc_no_seq, gain = acc_seq - acc_no_seq, n = nrow(df)),
      by_cluster = cluster_gains
    )
    
  }, error = function(e) NULL)
}


all_results <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  group_modify(~ {
    res <- run_seq_test_v3(.x)
    if (is.null(res)) return(tibble())
    res$overall
  }) %>%
  ungroup()

# Which clusters gain most from sequencing info across hitters?
cluster_summary <- filtered_data %>%
  filter(swing == TRUE, !is.na(cluster)) %>%
  group_by(batter_name, year) %>%
  group_modify(~ {
    res <- run_seq_test_v3(.x)
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

print(centroids)
