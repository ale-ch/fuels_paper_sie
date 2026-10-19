library(tidyverse)
library(purrr)
library(sf)
library(stringr)


geo_data <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/data/geo_data/geo_data.RDS")
data_path <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/results_run_nuts3_42_20261016_214911.RDS"
base_out_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation"
res_df <- readRDS(data_path)

# ------------------------------------------------------------------------------
# 1. SETUP & PATHS
# ------------------------------------------------------------------------------
geo_data_path <- "/Volumes/T7 Shield/FRES/fuels_data/data/geo_data/geo_data.RDS"
data_path     <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/results_run_nuts3_42_20261016_214911.RDS"
base_out_dir  <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/"

geo_data <- readRDS(geo_data_path)
res_df   <- readRDS(data_path)

# Extract flat df for hierarchy mapping
geo_data_df <- geo_data %>% st_drop_geometry()

# Merge hierarchy
res_df <- left_join(
  res_df %>% rename(nuts3 = region), 
  geo_data_df %>% select(nuts1, nuts2, nuts3) %>% filter(!duplicated(nuts3)),
  by = "nuts3"
)

# Output directory split strictly by model
ts_model_dir <- file.path(base_out_dir, "timeseries_by_model")

# Slide overlay metadata
run_window      <- unique(res_df$window)[1]
run_radius      <- unique(res_df$radius_m)[1]
run_granularity <- unique(res_df$estimation_granularity)[1]
meta_caption    <- sprintf("Rolling Window: %s | Spatial Radius: %s meters | Base Granularity: %s", 
                           run_window, run_radius, toupper(run_granularity))

# ------------------------------------------------------------------------------
# 2. AGGREGATIONS
# ------------------------------------------------------------------------------
res_nuts0 <- res_df %>%
  group_by(end_date, term, model) %>%
  summarize(estimate = mean(estimate, na.rm = TRUE), p_value = mean(p_value, na.rm = TRUE), .groups = "drop")

res_nuts1 <- res_df %>%
  group_by(end_date, term, model, nuts1) %>%
  summarize(estimate = mean(estimate, na.rm = TRUE), p_value = mean(p_value, na.rm = TRUE), .groups = "drop")

res_nuts2 <- res_df %>%
  group_by(end_date, term, model, nuts2) %>%
  summarize(estimate = mean(estimate, na.rm = TRUE), p_value = mean(p_value, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------------------------
# 3. CONFERENCE-READY PLOTTING FUNCTION
# ------------------------------------------------------------------------------
plot_single_model_ts <- function(data, color_col, title, subtitle, show_sig = TRUE) {
  
  data$end_date <- as.Date(data$end_date)
  data <- data %>% mutate(is_sig = ifelse(p_value < 0.05, "p < 0.05", "p >= 0.05"))
  
  p <- ggplot(data, aes(x = end_date, y = estimate, color = .data[[color_col]])) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
    geom_line(linewidth = 0.8, alpha = 0.75)
  
  if (show_sig) {
    p <- p + geom_point(aes(shape = is_sig), size = 2.5, alpha = 0.85) +
      scale_shape_manual(values = c("p < 0.05" = 19, "p >= 0.05" = 4))
  }
  
  num_colors <- length(unique(data[[color_col]]))
  if (num_colors <= 9) {
    p <- p + scale_color_brewer(palette = "Set1")
  } else {
    p <- p + scale_color_viridis_d(option = "turbo")
  }
  
  p <- p + theme_minimal(base_size = 14) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = meta_caption,
      x = "Rolling Window End Date",
      y = "Coefficient Estimate",
      color = str_to_title(color_col),
      shape = "Significance"
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 5, t = 10)),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40", margin = margin(b = 15)),
      plot.caption = element_text(hjust = 1, face = "italic", color = "grey50", size = 10, margin = margin(t = 15)),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# 4. GENERATION LOOP (SPLIT BY MODEL)
# ------------------------------------------------------------------------------
unique_terms  <- unique(res_df$term)[!is.na(unique(res_df$term))]
unique_models <- unique(res_df$model)[!is.na(unique(res_df$model))]

show_significance_markers <- TRUE # TOGGLE SIGNIFICANCE POINTS/CROSSES

# ==========================================
# A. CROSS-MODEL COMPARISON (NUTS-0 COMBINED)
# ==========================================
all_models_dir <- file.path(ts_model_dir, "ALL_MODELS", "NUTS0")
dir.create(all_models_dir, recursive = TRUE, showWarnings = FALSE)

walk(unique_terms, function(t) {
  safe_term <- str_replace_all(t, "[^A-Za-z0-9_.-]", "_")
  df_0_all  <- res_nuts0 %>% filter(term == t)
  
  if (nrow(df_0_all) > 0) {
    p_0_all <- plot_single_model_ts(
      df_0_all, "model", 
      paste("National (NUTS-0) Estimate Across All Models:", t), 
      "Cross-Model Comparison", 
      show_significance_markers
    )
    ggsave(file.path(all_models_dir, paste0("ts_N0_", safe_term, "_ALL_MODELS.png")), 
           plot = p_0_all, width = 12, height = 7, dpi = 300)
  }
})

# ==========================================
# B. PER-MODEL INDIVIDUAL SUBFOLDERS
# ==========================================
walk(unique_models, function(m) {
  safe_model <- str_replace_all(m, "[^A-Za-z0-9_.-]", "_")
  
  # Dedicated folder structure per model
  m_dir_n0 <- file.path(ts_model_dir, safe_model, "NUTS0")
  m_dir_n1 <- file.path(ts_model_dir, safe_model, "NUTS1")
  m_dir_n2 <- file.path(ts_model_dir, safe_model, "NUTS2")
  m_dir_n3 <- file.path(ts_model_dir, safe_model, "NUTS3_TopBottom")
  
  walk(c(m_dir_n0, m_dir_n1, m_dir_n2, m_dir_n3), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))
  
  walk(unique_terms, function(t) {
    safe_term <- str_replace_all(t, "[^A-Za-z0-9_.-]", "_")
    
    # 1. NUTS-0 (National)
    df_0 <- res_nuts0 %>% filter(model == m, term == t)
    if (nrow(df_0) > 0) {
      p_0 <- plot_single_model_ts(
        df_0, "model", 
        paste("National (NUTS-0) Estimate:", t), 
        paste("Model Specification:", m), 
        show_significance_markers
      )
      ggsave(file.path(m_dir_n0, paste0("ts_N0_", safe_term, ".png")), plot = p_0, width = 10, height = 6, dpi = 300)
    }
    
    # 2. NUTS-1 (Macro-Regions)
    df_1 <- res_nuts1 %>% filter(model == m, term == t)
    if (nrow(df_1) > 0) {
      p_1 <- plot_single_model_ts(
        df_1, "nuts1", 
        paste("Macro-Regional (NUTS-1) Trends:", t), 
        paste("Model Specification:", m, "| All Regions"), 
        show_significance_markers
      )
      ggsave(file.path(m_dir_n1, paste0("ts_N1_", safe_term, ".png")), plot = p_1, width = 12, height = 7, dpi = 300)
    }
    
    # 3. NUTS-2 (Regions)
    df_2 <- res_nuts2 %>% filter(model == m, term == t)
    if (nrow(df_2) > 0) {
      p_2 <- plot_single_model_ts(
        df_2, "nuts2", 
        paste("Regional (NUTS-2) Trends:", t), 
        paste("Model Specification:", m, "| All Regions"), 
        show_significance_markers
      ) + theme(legend.key.size = unit(0.35, "cm"), legend.text = element_text(size = 8))
      
      ggsave(file.path(m_dir_n2, paste0("ts_N2_", safe_term, ".png")), plot = p_2, width = 13, height = 7.5, dpi = 300)
    }
    
    # 4. NUTS-3 (Top 5 & Bottom 5 Extremes for this Specific Model & Term)
    n3_rank <- res_df %>% 
      filter(model == m, term == t) %>%
      group_by(nuts3) %>%
      summarize(avg_est = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
      filter(!is.na(avg_est)) %>%
      arrange(desc(avg_est))
    
    if (nrow(n3_rank) >= 10) {
      top_5 <- head(n3_rank$nuts3, 5)
      bot_5 <- tail(n3_rank$nuts3, 5)
      
      df_3 <- res_df %>% 
        filter(model == m, term == t, nuts3 %in% c(top_5, bot_5))
      
      p_3 <- plot_single_model_ts(
        df_3, "nuts3", 
        paste("Provincial (NUTS-3) Extremes:", t), 
        paste("Model Specification:", m, "| Top 5 & Bottom 5 Regions"), 
        show_significance_markers
      )
      
      ggsave(file.path(m_dir_n3, paste0("ts_N3_Extremes_", safe_term, ".png")), plot = p_3, width = 13, height = 7.5, dpi = 300)
    }
  })
})

cat("Model-split time series plots successfully saved to:", ts_model_dir, "\n")