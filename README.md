# SMGT430 Baseball Analytics Project

A comprehensive baseball swing analysis project using Statcast/TrackMan data to analyze hitter swing patterns, predict pitch types, and cluster swing qualities.

## Project Overview

This project analyzes MLB swing data from the 2024-2025 seasons to:
- **Cluster hitters** by swing characteristics (5 swing types)
- **Predict pitch types** based on game context and sequence
- **Analyze swing quality** at both league-wide and player-specific levels

## Repository Structure

```
SMGT490_project/
├── 00_download.R      # Download raw Statcast data from Baseball Savant
├── 01_clean.R    # Clean data, filter swings, create derived metrics
├── 02_clustering.R   # Gaussian Mixture Model clustering
├── 03_pitch_prediction.R  # Random forest pitch type prediction
├── 04_eda.R               # Exploratory data analysis & player comparisons
├── 05_league_wide_swing_predictionss.R       # League-wide swing pattern analysis
├── 06_player_analysis.R    # Player-specific swing quality models
├── run_all.R    # Runs all scripts
├── Final Paper.pdf    # Final Paper
├── README.md
├── SMGT490_project.Rproj
├── data/                  # Input/output data files
│   ├── savant_data_2024.csv
│   ├── savant_data_2025.csv
│   ├── swing_data_2024.csv
│   ├── swing_data_2025.csv
│   ├── savant_data_with_clusters_2024.csv
│   ├── savant_data_with_clusters_2025.csv
│   ├── savant_data_with_clusters_2024_2025.csv
│   └── savant_data_with_clusters_and_preds.csv
├── app_materials/         # Shiny app components
│   ├── train_models.R
│   └── shiny_app/
│       ├── app.R
│       └── bundle.rds
└── old_materials #Script drafts      
```

## Execution Order

Run scripts in numerical order (00 → 06) or `run run_all.R`


## Key Dependencies

```r
# Core analysis
library(tidyverse)
library(ranger)
library(mclust)
library(gt)
library(patchwork)

# Data acquisition
library(sabRmetrics)

# Visualization
library(ggplot2)
library(factoextra)
```

## Data Sources

- **Baseball Savant** (via sabRmetrics): Raw Statcast data


## Output

- **Clustered data**: `savant_data_with_clusters_*.csv`
- **Predicted pitches**: `savant_data_with_clusters_and_preds.csv`
- **Shiny app**: Interactive player analysis (see `app_materials/`)
