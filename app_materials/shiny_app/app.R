library(shiny)
library(tidyverse)
library(ranger)

# ── Load only small metadata at startup ───────────────────────────────────────
meta_bundle <- readRDS("bundle_meta.rds")
metadata    <- meta_bundle$metadata
centroids   <- meta_bundle$centroids

# Helper: convert batter name → filename
batter_to_file <- function(nm) {
  clean <- gsub("[^A-Za-z0-9_]", "_", nm)
  file.path("models", paste0(clean, ".rds"))
}

# ── Statcast zone layout (1-14) ───────────────────────────────────────────────
zone_coords <- tribble(
  ~zone, ~x_min, ~x_max, ~y_min, ~y_max,
  1,  -0.95,  -0.32,  2.33,  3.17,
  2,  -0.32,   0.32,  2.33,  3.17,
  3,   0.32,   0.95,  2.33,  3.17,
  4,  -0.95,  -0.32,  1.50,  2.33,
  5,  -0.32,   0.32,  1.50,  2.33,
  6,   0.32,   0.95,  1.50,  2.33,
  7,  -0.95,  -0.32,  0.67,  1.50,
  8,  -0.32,   0.32,  0.67,  1.50,
  9,   0.32,   0.95,  0.67,  1.50,
  11, -1.75,   0.00,  3.17,  4.00,
  12,  0.00,   1.75,  3.17,  4.00,
  13, -1.75,   0.00,  0.00,  0.67,
  14,  0.00,   1.75,  0.00,  0.67,
  10, -1.75,   1.75,  0.33,  4.00
)

# ── UI (unchanged) ────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    body { background-color: #f8f9fa; font-family: 'Georgia', serif; }
    .app-title { font-size: 1.6em; font-weight: bold; color: #1a3a5c; margin-bottom: 4px; }
    .app-subtitle { color: #666; font-size: 0.95em; margin-bottom: 12px; }
    .instructions-box {
      background: #eef4fb; border-left: 4px solid #1565C0; border-radius: 6px;
      padding: 14px 18px; margin-bottom: 18px; font-size: 0.91em; color: #2c3e50;
    }
    .instructions-box h5 { margin-top: 0; color: #1565C0; font-size: 1em; font-weight: bold; }
    .instructions-box ol { margin: 6px 0 0 0; padding-left: 20px; }
    .instructions-box li { margin-bottom: 5px; }
    .instructions-toggle {
      background: none; border: none; color: #1565C0; font-size: 0.88em;
      cursor: pointer; padding: 0; text-decoration: underline; margin-bottom: 8px;
    }
    .section-header {
      font-size: 1em; font-weight: bold; color: #1a3a5c;
      border-bottom: 2px solid #1565C0; padding-bottom: 4px; margin-bottom: 12px;
    }
    .heatmap-hero .section-header { font-size: 1.4em; }
    .sidebar-card {
      background: #fff; border-radius: 8px; padding: 16px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
    }
    .sidebar-card h5 {
      color: #1a3a5c; font-weight: bold; margin-top: 12px; margin-bottom: 6px;
      font-size: 0.95em; text-transform: uppercase; letter-spacing: 0.03em;
    }
    .sidebar-card h5:first-child { margin-top: 0; }
    .plot-card {
      background: #fff; border-radius: 8px; padding: 16px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08); margin-bottom: 16px;
    }
    .heatmap-hero {
      background: #fff; border-radius: 10px; padding: 20px;
      box-shadow: 0 2px 8px rgba(21,101,192,0.10); border-top: 3px solid #1565C0;
      margin-bottom: 18px;
    }
    .bottom-panel {
      background: #fff; border-radius: 8px; padding: 16px;
      box-shadow: 0 1px 4px rgba(0,0,0,0.08);
    }
  "))),
  
  div(class = "app-title", "Pitch Sequencing — Swing Quality Predictor"),
  div(class = "app-subtitle",
      "Predict how a batter is likely to swing based on pitch type, location, count, and sequence context."),
  
  tags$button(
    id = "toggle-instructions", class = "instructions-toggle",
    onclick = "
      var box = document.getElementById('instructions-box');
      var btn = document.getElementById('toggle-instructions');
      if (box.style.display === 'none') {
        box.style.display = 'block'; btn.textContent = '▲ Hide instructions';
      } else {
        box.style.display = 'none'; btn.textContent = '▼ Show instructions';
      }
    ",
    "▼ Show instructions"
  ),
  
  div(
    id = "instructions-box", class = "instructions-box", style = "display: none;",
    tags$h5("How to Use This App"),
    tags$ol(
      tags$li(tags$b("Select a Batter"), " — choose a hitter from the dropdown."),
      tags$li(tags$b("Set the Situation"), " — fill in count, outs, and pitcher handedness."),
      tags$li(tags$b("Enter the Previous Pitch"), " — optionally specify last pitch type, zone, and whether batter swung."),
      tags$li(tags$b("Choose the Next Pitch"), " — select the pitch type you are considering."),
      tags$li(tags$b("Read the Heatmap"), " — darker blue = higher probability of the highlighted cluster."),
      tags$li(tags$b("Pick a Cluster"), " — use the selector to switch which swing cluster is shown."),
      tags$li(tags$b("Bar Chart"), " — overall cluster probabilities averaged across zones."),
      tags$li(tags$b("Centroid Table"), " — cluster contact rate and expected wOBA.")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "sidebar-card",
          tags$h5("Batter"),
          selectInput("batter", NULL, choices = metadata$batters, selected = metadata$batters[1]),
          
          tags$h5("Situation"),
          selectInput("count", "Count", choices = metadata$counts, selected = "0-0"),
          numericInput("outs", "Outs", value = 0, min = 0, max = 2),
          selectInput("pitch_hand", "Pitcher Hand", choices = c("R", "L"), selected = "R"),
          
          tags$h5("Previous Pitch"),
          selectInput("prev_pitch_name", "Pitch Type",
                      choices = c("None", metadata$prev_pitch_names), selected = "None"),
          selectInput("prev_zone", "Zone",
                      choices = c("None", as.character(metadata$zones)), selected = "None"),
          checkboxInput("prev_swing", "Batter swung?", value = FALSE),
          
          tags$h5("Next Pitch"),
          selectInput("pitch_name", "Pitch Type",
                      choices = metadata$pitch_names, selected = metadata$pitch_names[1]),
          
          tags$h5("Highlight Cluster"),
          selectInput("selected_cluster", NULL,
                      choices = as.character(metadata$clusters),
                      selected = as.character(metadata$clusters[1]))
      )
    ),
    
    mainPanel(
      width = 9,
      
      div(class = "heatmap-hero",
          div(class = "section-header", textOutput("heatmap_title", inline = TRUE)),
          fluidRow(
            column(9, plotOutput("zone_heatmap", height = "400px")),
            column(3,
                   div(style = "padding-top: 20px;",
                       tags$p(style = "font-size: 0.8em; color: #666; margin-bottom: 6px; text-align: center;",
                              "Zone Reference"),
                       plotOutput("zone_reference", height = "280px")
                   )
            )
          )
      ),
      
      fluidRow(
        column(5,
               div(class = "bottom-panel",
                   div(class = "section-header", "Swing Cluster Probabilities"),
                   plotOutput("bar_chart", height = "280px")
               )
        ),
        column(7,
               div(class = "bottom-panel",
                   div(class = "section-header", "Cluster Centroids"),
                   tableOutput("centroid_table")
               )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Lazy model cache: one model in memory at a time ─────────────────────────
  cached_model  <- reactiveVal(NULL)
  cached_batter <- reactiveVal(NULL)
  
  active_model <- reactive({
    req(input$batter)
    if (!identical(input$batter, cached_batter())) {
      f <- batter_to_file(input$batter)
      validate(need(file.exists(f), paste("Model file not found:", f)))
      cached_model(readRDS(f))
      cached_batter(input$batter)
      gc()  # release the previous model's memory
    }
    cached_model()
  })
  
  # Update cluster selector when batter changes
  observe({
    req(input$batter)
    batter_clusters <- centroids %>%
      filter(batter_name == input$batter) %>%
      pull(cluster) %>%
      as.character() %>%
      sort()
    updateSelectInput(session, "selected_cluster",
                      choices = batter_clusters,
                      selected = batter_clusters[1])
  })
  
  # ── Build a new_data row ─────────────────────────────────────────────────────
  make_row <- function(zone_val) {
    tibble(
      pitch_hand      = input$pitch_hand,
      count           = input$count,
      outs            = as.integer(input$outs),
      prev_pitch_name = factor(input$prev_pitch_name,
                               levels = c("None", metadata$prev_pitch_names)),
      prev_swing      = as.integer(input$prev_swing),
      prev_zone       = factor(input$prev_zone,
                               levels = c("None", as.character(metadata$zones))),
      pitch_name      = factor(input$pitch_name,
                               levels = metadata$pitch_names),
      zone            = factor(zone_val, levels = as.character(metadata$zones))
    )
  }
  
  # ── Core predictions (zone 5 / overall) ─────────────────────────────────────
  get_predictions <- reactive({
    model <- active_model()
    req(model)
    pred  <- predict(model, data = make_row("5"))
    probs <- pred$predictions[1, ]
    tibble(cluster = names(probs), probability = as.numeric(probs)) %>%
      arrange(desc(probability))
  })
  
  # ── Heatmap data across all 13 zones ─────────────────────────────────────────
  get_heatmap_data <- reactive({
    model <- active_model()
    req(model, input$selected_cluster)
    
    real_zones <- as.character(metadata$zones[metadata$zones != 10])
    
    # Batch: one predict call with all 13 rows
    all_rows <- map_dfr(real_zones, make_row)
    pred     <- predict(model, data = all_rows)
    
    map_dfr(seq_along(real_zones), function(i) {
      probs <- pred$predictions[i, ]
      p <- if (input$selected_cluster %in% names(probs)) probs[[input$selected_cluster]] else NA_real_
      tibble(zone = as.integer(real_zones[i]), prob = p)
    })
  })
  
  # ── Outputs ──────────────────────────────────────────────────────────────────
  output$heatmap_title <- renderText({
    paste0("Zone Heatmap — P(Cluster ", input$selected_cluster, " | Location)")
  })
  
  output$bar_chart <- renderPlot({
    preds <- get_predictions()
    ggplot(preds, aes(x = reorder(cluster, probability), y = probability, fill = probability)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = scales::percent(probability, accuracy = 0.1)),
                hjust = -0.1, size = 3.5) +
      scale_fill_gradient(low = "#d0e8f5", high = "#1565C0", guide = "none") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      coord_flip() +
      labs(x = "Swing Cluster", y = "Probability") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.y = element_blank())
  })
  
  output$zone_heatmap <- renderPlot({
    hmap  <- get_heatmap_data()
    zones <- zone_coords %>% left_join(hmap, by = "zone") %>% filter(zone != 10)
    
    filler <- bind_rows(
      tibble(zone = 11L, x_min = -1.75, x_max = -0.95, y_min = 1.915, y_max = 3.17),
      tibble(zone = 12L, x_min =  0.95, x_max =  1.75, y_min = 1.915, y_max = 3.17),
      tibble(zone = 13L, x_min = -1.75, x_max = -0.95, y_min = 0.67,  y_max = 1.915),
      tibble(zone = 14L, x_min =  0.95, x_max =  1.75, y_min = 0.67,  y_max = 1.915)
    ) %>% left_join(hmap, by = "zone")
    
    ggplot(zones) +
      geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = prob),
                color = "white", linewidth = 0.8) +
      geom_rect(data = filler,
                aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = prob),
                color = NA, linewidth = 0.8) +
      geom_text(data = zones,
                aes(x = (x_min + x_max) / 2, y = (y_min + y_max) / 2,
                    label = ifelse(!is.na(prob), scales::percent(prob, accuracy = 1), ""),
                    color = prob > 0.5),
                size = 6.5, fontface = "bold") +
      geom_text(data = zones %>%
                  mutate(
                    lx = case_when(zone %in% c(12, 14) ~ x_max - 0.08, TRUE ~ x_min + 0.08),
                    ly = case_when(zone %in% c(13, 14) ~ y_min + 0.10, TRUE ~ y_max - 0.10),
                    ha = case_when(zone %in% c(12, 14) ~ 1,            TRUE ~ 0)
                  ),
                aes(x = lx, y = ly, label = zone, hjust = ha, color = prob > 0.5),
                size = 2.8, vjust = 1) +
      annotate("rect", xmin = -0.95, xmax = 0.95, ymin = 0.67, ymax = 3.17,
               fill = NA, color = "black", linewidth = 1.4) +
      scale_fill_gradient(low = "#eaf4fb", high = "#1565C0", na.value = "grey85",
                          labels = scales::percent,
                          name = paste0("P(Cluster ", input$selected_cluster, ")")) +
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "white"), guide = "none") +
      coord_fixed(xlim = c(-1.9, 1.9), ylim = c(-0.1, 4.1)) +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid = element_blank(), legend.position = "right",
            axis.text = element_blank(), axis.ticks = element_blank())
  })
  
  output$centroid_table <- renderTable({
    centroids %>%
      filter(batter_name == input$batter) %>%
      select(cluster, n, contact_rate, expected_woba) %>%
      mutate(
        cluster       = as.character(cluster),
        contact_rate  = scales::percent(contact_rate, accuracy = 0.1),
        expected_woba = sprintf("%.3f", expected_woba)
      ) %>%
      arrange(desc(expected_woba)) %>%
      rename("Cluster" = cluster, "N Swings" = n,
             "Contact %" = contact_rate, "xwOBA" = expected_woba)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$zone_reference <- renderPlot({
    ref_zones <- tribble(
      ~zone, ~x_min, ~x_max, ~y_min, ~y_max,
      1, -0.95, -0.32, 2.33, 3.17,  2, -0.32, 0.32, 2.33, 3.17,
      3,  0.32,  0.95, 2.33, 3.17,  4, -0.95,-0.32, 1.50, 2.33,
      5, -0.32,  0.32, 1.50, 2.33,  6,  0.32, 0.95, 1.50, 2.33,
      7, -0.95, -0.32, 0.67, 1.50,  8, -0.32, 0.32, 0.67, 1.50,
      9,  0.32,  0.95, 0.67, 1.50
    )
    l_polys <- bind_rows(
      tibble(zone=11, x=c(-1.75,0.00,0.00,-0.95,-0.95,-1.75), y=c(4.00,4.00,3.17,3.17,1.915,1.915)),
      tibble(zone=12, x=c(0.00,1.75,1.75,0.95,0.95,0.00),     y=c(4.00,4.00,1.915,1.915,3.17,3.17)),
      tibble(zone=13, x=c(-1.75,-0.95,-0.95,0.00,0.00,-1.75), y=c(1.915,1.915,0.67,0.67,0.00,0.00)),
      tibble(zone=14, x=c(0.00,0.95,0.95,1.75,1.75,0.00),     y=c(0.67,0.67,1.915,1.915,0.00,0.00))
    )
    l_labels <- tibble(
      zone=c(11,12,13,14), x=c(-1.67,1.67,-1.67,1.67),
      y=c(3.90,3.90,0.10,0.10), hjust=c(0,1,0,1)
    )
    ggplot(ref_zones) +
      geom_rect(aes(xmin=x_min,xmax=x_max,ymin=y_min,ymax=y_max),
                fill="white", color="grey50", linewidth=0.5) +
      geom_polygon(data=l_polys, aes(x=x,y=y,group=zone),
                   fill="white", color="grey50", linewidth=0.5) +
      geom_text(aes(x=(x_min+x_max)/2, y=(y_min+y_max)/2, label=zone),
                size=3.5, color="grey30", fontface="bold") +
      geom_text(data=l_labels, aes(x=x,y=y,label=zone,hjust=hjust),
                size=3.5, color="grey30", fontface="bold") +
      annotate("rect", xmin=-0.95, xmax=0.95, ymin=0.67, ymax=3.17,
               fill=NA, color="black", linewidth=1.2) +
      coord_fixed(xlim=c(-1.9,1.9), ylim=c(-0.1,4.1)) +
      labs(x=NULL, y=NULL) +
      theme_void() +
      theme(panel.background=element_rect(fill="white", color=NA))
  })
}

shinyApp(ui, server)