library(tidyverse)
library(tidyverse)
library(purrr)
library(sf)

geo_data <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/data/geo_data/geo_data.RDS")
data_path <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/results_run_nuts3_42_20261016_214911.RDS"
base_out_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation"
res_df <- readRDS(data_path)


geo_data_df <- geo_data %>% 
  st_drop_geometry() 

res_df <- left_join(
  res_df %>% rename(nuts3 = region), 
  geo_data_df %>% select(nuts1, nuts2, nuts3) %>% filter(!duplicated(nuts3)),
  by = "nuts3"
)


library(tidyverse)
library(purrr)
library(sf)
library(stringr)

# ------------------------------------------------------------------------------
# 1. SETUP & PATHS
# ------------------------------------------------------------------------------
# (Assuming geo_data and res_df are already loaded and joined)
# base_out_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/maps"
ts_out_dir <- file.path(base_out_dir, "timeseries")

walk(c("NUTS0", "NUTS1", "NUTS2", "NUTS3_TopBottom"), 
     ~dir.create(file.path(ts_out_dir, .x), recursive = TRUE, showWarnings = FALSE))

# Extract model run details to overlay on all plots
run_window <- unique(res_df$window)[1]
run_radius <- unique(res_df$radius_m)[1]
run_granularity <- unique(res_df$estimation_granularity)[1]
meta_caption <- sprintf("Rolling Window: %s | Spatial Radius: %s meters | Base Granularity: %s", 
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
plot_conference_ts <- function(data, color_col, title, subtitle, show_sig = TRUE, facet_col = NULL) {
  
  data$end_date <- as.Date(data$end_date)
  data <- data %>% mutate(is_sig = ifelse(p_value < 0.05, "p < 0.05", "p >= 0.05"))
  
  p <- ggplot(data, aes(x = end_date, y = estimate, color = .data[[color_col]])) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.6) +
    geom_line(linewidth = 0.8, alpha = 0.7)
  
  if (show_sig) {
    p <- p + geom_point(aes(shape = is_sig), size = 2.5, alpha = 0.8) +
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
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      strip.background = element_rect(fill = "grey95", color = NA)
    )
  
  if (!is.null(facet_col)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_col)), scales = "free_y")
  }
  
  return(p)
}

# ------------------------------------------------------------------------------
# 4. GENERATION LOOP (CLEANED)
# ------------------------------------------------------------------------------
unique_terms <- unique(res_df$term)
unique_terms <- unique_terms[!is.na(unique_terms)] # Drop NAs

unique_models <- unique(res_df$model)
show_significance_markers <- TRUE 

walk(unique_terms, function(t) {
  
  safe_term <- str_replace_all(t, "[^A-Za-z0-9_.-]", "_")
  
  # ==========================================
  # NUTS0 (National)
  # ==========================================
  df_0 <- res_nuts0 %>% filter(term == t)
  
  if (nrow(df_0) > 0) {
    p_0_all <- plot_conference_ts(df_0, "model", paste("National (NUTS-0) Estimate:", t), "Comparison Across Models", show_significance_markers)
    ggsave(file.path(ts_out_dir, "NUTS0", paste0("ts_N0_", safe_term, "_ALL_MODELS.png")), plot = p_0_all, width = 12, height = 7, dpi = 300)
    
    walk(unique_models, function(m) {
      df_0_m <- df_0 %>% filter(model == m)
      if(nrow(df_0_m) > 0) {
        p_0_m <- plot_conference_ts(df_0_m, "model", paste("National (NUTS-0) Estimate:", t), paste("Model:", m), show_significance_markers)
        ggsave(file.path(ts_out_dir, "NUTS0", paste0("ts_N0_", safe_term, "_", m, ".png")), plot = p_0_m, width = 10, height = 6, dpi = 300)
      }
    })
  }
  
  # ==========================================
  # NUTS1
  # ==========================================
  df_1 <- res_nuts1 %>% filter(term == t)
  if(nrow(df_1) > 0) {
    p_1 <- plot_conference_ts(df_1, "nuts1", paste("Macro-Regional (NUTS-1) Trends:", t), "All Regions | Faceted by Model", show_significance_markers, facet_col = "model")
    ggsave(file.path(ts_out_dir, "NUTS1", paste0("ts_N1_", safe_term, "_faceted.png")), plot = p_1, width = 14, height = 8, dpi = 300)
  }
  
  # ==========================================
  # NUTS2
  # ==========================================
  df_2 <- res_nuts2 %>% filter(term == t)
  if(nrow(df_2) > 0) {
    p_2 <- plot_conference_ts(df_2, "nuts2", paste("Regional (NUTS-2) Trends:", t), "All Regions | Faceted by Model", show_significance_markers, facet_col = "model") +
      theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 9)) 
    ggsave(file.path(ts_out_dir, "NUTS2", paste0("ts_N2_", safe_term, "_faceted.png")), plot = p_2, width = 16, height = 9, dpi = 300)
  }
  
  # ==========================================
  # NUTS3 (Top 5 & Bottom 5)
  # ==========================================
  n3_rank <- res_df %>% 
    filter(term == t) %>%
    group_by(nuts3) %>%
    summarize(avg_est = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(avg_est)) %>%
    arrange(desc(avg_est))
  
  if(nrow(n3_rank) >= 10) {
    top_5 <- head(n3_rank$nuts3, 5)
    bot_5 <- tail(n3_rank$nuts3, 5)
    
    df_3 <- res_df %>% 
      filter(term == t, nuts3 %in% c(top_5, bot_5)) %>%
      mutate(Classification = factor(ifelse(nuts3 %in% top_5, "Top 5 Estimates", "Bottom 5 Estimates"), 
                                     levels = c("Top 5 Estimates", "Bottom 5 Estimates")))
    
    p_3 <- plot_conference_ts(df_3, "nuts3", paste("Provincial (NUTS-3) Extremes:", t), "Highest 5 vs Lowest 5 Regions | Faceted by Model", show_significance_markers, facet_col = "model") +
      facet_grid(Classification ~ model, scales = "free_y")
    
    ggsave(file.path(ts_out_dir, "NUTS3_TopBottom", paste0("ts_N3_Extremes_", safe_term, "_faceted.png")), plot = p_3, width = 16, height = 10, dpi = 300)
  }
})

cat("Conference-ready time series plots successfully generated in:", ts_out_dir, "\n")