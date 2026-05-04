# SMGT430 Baseball Analytics Project

A comprehensive baseball swing analysis project using Statcast/TrackMan data to analyze hitter swing patterns, predict pitch types, and cluster swing qualities.

## Project Overview

This project analyzes MLB swing data from the 2024-2025 seasons to:
- **Cluster hitters** by swing characteristics (5 swing types)
- **Predict pitch types** based on game context and sequence
- **Analyze swing quality** at both league-wide and player-specific levels

## Repository Structure

```
SMGT430_project/
├── 01_data_download.R      # Download raw Statcast data from Baseball Savant
├── 02_data_processing.R    # Clean data, filter swings, create derived metrics
├── 03_eda.R               # Exploratory data analysis & player comparisons
├── 04_hitter_clusters.R   # Gaussian Mixture Model clustering
├── 05_pitch_prediction.R  # Random forest pitch type prediction
├── 06_league_wide.R       # League-wide swing pattern analysis
├── 07_final_modeling.R    # Player-specific swing quality models
├── 08_plots_and_tables.R  # Visualizations and summary tables
├── README.md
├── SMGT430_project.Rproj
├── Data/                  # Input/output data files
│   ├── savant_data_2024.csv
│   ├── savant_data_2025.csv
│   ├── swing_data_2024.csv
│   ├── swing_data_2025.csv
│   ├── savant_data_with_clusters_2024.csv
│   ├── savant_data_with_clusters_2025.csv
│   ├── savant_data_with_clusters_2024_2025.csv
│   └── savant_data_with_clusters_and_preds.csv
└── app_materials/         # Shiny app components
    ├── train_models.R
    └── shiny_app/
        ├── app.R
        └── bundle.rds
```

## Execution Order

Run scripts in numerical order (01 → 08):

| Step | Script | Description |
|------|--------|-------------|
| 1 | `01_data_download.R` | Download Statcast data via sabRmetrics |
| 2 | `02_data_processing.R` | Filter swings, add TrackMan metrics |
| 3 | `03_eda.R` | Explore player swing metrics |
| 4 | `04_hitter_clusters.R` | Cluster hitters using GMM |
| 5 | `05_pitch_prediction.R` | Predict pitch types with random forest |
| 6 | `06_league_wide.R` | Analyze league-wide patterns |
| 7 | `07_final_modeling.R` | Build player-specific models |
| 8 | `08_plots_and_tables.R` | Generate visualizations |

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
- **TrackMan**: Swing metrics (bat speed, attack angle, swing path tilt)
- **Seasons**: 2024, 2025

## Key Metrics

| Metric | Description |
|--------|-------------|
| `bat_speed` | Bat velocity at impact (mph) |
| `swing_length` | Total swing distance (ft) |
| `attack_angle` | Angle of bat relative to horizontal (°) |
| `swing_path_tilt` | Vertical bat path angle (°) |
| `squared_up` | Quality contact (launch_speed / bat_speed > 0.8) |
| `blast` | Elite contact (composite score > 164) |

## Cluster Definitions

The GMM clustering identifies 5 swing types based on:
- Bat speed
- Swing length
- Attack angle
- Swing path tilt
- Contact point

## Players Analyzed

- Aaron Judge
- Shohei Ohtani
- Yandy Díaz
- Bobby Witt Jr.
- Luis Arraez
- Mike Trout

## Output

- **Clustered data**: `savant_data_with_clusters_*.csv`
- **Predicted pitches**: `savant_data_with_clusters_and_preds.csv`
- **Shiny app**: Interactive player analysis (see `app_materials/`)

## License

This project is for educational purposes as part of SMGT430.