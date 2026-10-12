# ------------------------------------------------------------------------------
# 4. DIAGNOSTICS & COMPARISONS (UPDATED FOR DYNAMIC REGIONS)
# ------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggplot2)
library(jsonlite)

# --- REFACTOR NOTE: Merged the two redundant functions into a single robust standalone function. ---
# --- REFACTOR NOTE: Replaced hardcoded 'nuts1' and 'nuts0' with a generic 'region' column. ---

analyze_standalone_results <- function(parts_dir, is_split = TRUE) {
  file_paths <- list.files(parts_dir, pattern = "\\.RDS$", full.names = TRUE)
  if(length(file_paths) == 0) stop("No RDS files found in the specified directory.")
  
  coef_df <- map_dfr(file_paths, function(file_path) {
    mod_data <- readRDS(file_path)
    
    c_df_part <- map_dfr(names(mod_data$models), function(m_name) {
      summ <- summary(mod_data$models[[m_name]])
      c_df <- as.data.frame(summ$CoefTable)
      colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
      c_df$term <- rownames(c_df)
      c_df$model <- m_name
      c_df$window <- mod_data$window
      c_df$end_week <- mod_data$end_week
      c_df$region <- mod_data$region # Extracted dynamically from the saved model object
      c_df$date <- as.Date(paste0(mod_data$end_week, "-1"), format = "%Y-%W-%u")
      return(c_df)
    })
    
    rm(mod_data)
    gc()
    return(c_df_part)
  })
  
  rownames(coef_df) <- NULL
  
  coef_df <- coef_df %>% mutate(term = case_when(
    grepl("lambda|rho", term, ignore.case = TRUE) ~ "Spatial Lag (rho)",
    grepl("mean_price_gasoline_self", term) ~ "Time Lag (Gasoline)",
    grepl("brent_price_mean", term) ~ "Time Lag (Brent)",
    grepl("brent_diff_pos", term) ~ "Time Lag (Brent Δ+)",
    grepl("brent_diff_neg", term) ~ "Time Lag (Brent Δ-)",
    TRUE ~ term
  ))
  
  return(coef_df)
}

# --- REFACTOR NOTE: Now facets by the generic 'region' column. ---
plot_comparative_timeseries <- function(coef_df, target_term, title, is_split = TRUE) {
  plot_data <- coef_df %>% filter(term == target_term)
  if (nrow(plot_data) == 0) return(ggplot() + theme_void() + labs(title = paste("Missing data for", target_term)))
  
  p <- ggplot(plot_data, aes(x = date, y = estimate, color = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line(linewidth = 0.8) +
    theme_minimal() +
    labs(title = title, x = "Date", y = "Estimate", color = "Model")
  
  if (is_split) p <- p + facet_wrap(~ region, scales = "free_y")
  return(p)
}

# ------------------------------------------------------------------------------
# 5. SUMMARY AND MAP GENERATION FUNCTIONS
# ------------------------------------------------------------------------------

# --- REFACTOR NOTE: Grouping is now done by 'region' instead of 'nuts1'. ---
generate_summary_table <- function(stats_data) {
  summary_table <- stats_data %>%
    group_by(region, term, model) %>%
    summarize(
      mean_estimate = mean(estimate, na.rm = TRUE),
      median_estimate = median(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      mean_p_value = mean(p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(region, term, model)
  
  return(summary_table)
}

# --- REFACTOR NOTE: Added 'target_nuts_level' parameter to dynamically join and group 
# --- geometries by nuts1, nuts2, or nuts3 depending on the user's pipeline choice. ---
plot_coefficient_maps <- function(summary_table, shp_path, nuts_path, 
                                  target_nuts_level = "nuts1", target_model = "SDM", 
                                  target_term = "Spatial Lag (rho)") {
  
  # 1. Load and prepare geographic bounds
  nuts_data <- as.data.frame(fromJSON(nuts_path)$resultset) %>% 
    select(PRO_COM, contains("DEN")) %>% 
    rename(nuts1 = DEN_RIP, nuts2 = DEN_REG, nuts3 = DEN_UTS)
  
  shp_data <- st_read(shp_path, quiet = TRUE) %>%
    left_join(nuts_data, by = c("PRO_COM" = "PRO_COM"))
  
  # 2. Aggregate municipal geometries dynamically based on target_nuts_level
  # Convert the string to a symbol so dplyr can group by it
  level_sym <- sym(target_nuts_level)
  
  region_polygons <- shp_data %>%
    group_by(!!level_sym) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    rename(region = !!level_sym) # Standardize column name to match summary_table
  
  # 3. Filter statistical data
  stats_subset <- summary_table %>% filter(model == target_model, term == target_term)
  
  # 4. Join geometries with the model estimates
  map_data <- region_polygons %>% left_join(stats_subset, by = "region")
  
  # 5. Generate Map
  p_map <- ggplot(data = map_data) +
    geom_sf(aes(fill = mean_estimate), color = "white", linewidth = 0.3) +
    scale_fill_viridis_c(option = "magma", na.value = "grey80") +
    theme_void() +
    labs(
      title = sprintf("Geographic Distribution: %s", target_term),
      subtitle = sprintf("Model: %s | Aggregation: %s", target_model, toupper(target_nuts_level)),
      fill = "Mean\nEstimate"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right"
    )
  
  return(list(plot = p_map, map_data = map_data))
}

# ------------------------------------------------------------------------------
# 6. PIPELINE WRAPPER AND SUMMARY PLOTTER
# ------------------------------------------------------------------------------

# --- REFACTOR NOTE: Updated to check for 'region' instead of 'nuts1' for dynamic faceting. ---
plot_summary_statistics <- function(summary_table, terms_to_plot = NULL) {
  plot_data <- summary_table %>% filter(term != "(Intercept)")
  
  if (!is.null(terms_to_plot)) {
    plot_data <- plot_data %>% filter(term %in% terms_to_plot)
  }
  
  p <- ggplot(plot_data, aes(y = reorder(model, mean_estimate), x = mean_estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_linerange(aes(xmin = mean_estimate - sd_estimate, xmax = mean_estimate + sd_estimate), 
                   linewidth = 1.2, alpha = 0.6) +
    geom_point(size = 3.5) +
    geom_point(aes(x = median_estimate), size = 1.5, shape = 21, fill = "white", stroke = 1) +
    theme_minimal() +
    labs(
      title = "Coefficient Stability and Central Tendency",
      subtitle = "Solid dot = Mean | White dot = Median | Lines = ±1 SD",
      x = "Coefficient Estimate",
      y = "Model Specification"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11),
      axis.text.y = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank()
    )
  
  if ("region" %in% colnames(plot_data) && length(unique(plot_data$region)) > 1) {
    p <- p + facet_grid(region ~ term, scales = "free_x")
  } else {
    p <- p + facet_wrap(~ term, scales = "free_x", ncol = 2)
  }
  
  return(p)
}

