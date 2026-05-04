library(sabRmetrics)
library(tidyverse)

cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2024-01-01",
  end_date   = "2024-12-31",
  cl         = cluster
)
parallel::stopCluster(cluster)
write_csv(data_baseballsavant, "data/savant_data_2024.csv")

cluster <- parallel::makeCluster(parallel::detectCores())
data_baseballsavant <- sabRmetrics::download_baseballsavant(
  start_date = "2025-01-01",
  end_date   = "2025-12-31",
  cl         = cluster
)
parallel::stopCluster(cluster)
write_csv(data_baseballsavant, "data/savant_data_2025.csv")