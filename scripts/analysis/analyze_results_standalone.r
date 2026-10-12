# ------------------------------------------------------------------------------
# STANDALONE DIAGNOSTICS & VISUALIZATION SUITE
# ------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(jsonlite)
library(ggplot2)

# 1. Standalone Analysis Function (Reads directly from RDS directory)
analyze_standalone_results <- function(parts_dir, split_by_nuts1 = TRUE) {
  file_paths <- list.files(parts_dir, pattern = "\\.RDS$", full.names = TRUE)
  
  if(length(file_paths) == 0) stop("No RDS files found in the specified directory.")
  
  cat(sprintf("Files found: %d\n", length(file_paths)))
  
  coef_df <- map_dfr(seq_along(file_paths), function(i) {
    file_path <- file_paths[i]
    mod_data <- readRDS(file_path)
    
    c_df_part <- map_dfr(names(mod_data$models), function(m_name) {
      
      start_time <- Sys.time()
      
      summ <- summary(mod_data$models[[m_name]])
      
      c_df <- as.data.frame(summ$CoefTable)
      colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
      c_df$term <- rownames(c_df)
      c_df$model <- m_name
      c_df$window <- mod_data$window
      c_df$end_week <- mod_data$end_week
      c_df$nuts1 <- mod_data$region
      c_df$date <- as.Date(paste0(mod_data$end_week, "-1"), format = "%Y-%W-%u")
      
      end_time <- Sys.time()
      exec_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      cat(sprintf("Processed File: %d/%d | Model: %s | Execution time: %.10f seconds\n", 
                  i, length(file_paths), m_name, exec_time))
      
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

# 2. Summary Table Generator
generate_summary_table <- function(stats_data) {
  summary_table <- stats_data %>%
    group_by(nuts1, term, model) %>%
    summarize(
      mean_estimate = mean(estimate, na.rm = TRUE),
      median_estimate = median(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      mean_p_value = mean(p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(nuts1, term, model)
  
  return(summary_table)
}

# 3. Time-Series Plotter
plot_comparative_timeseries <- function(coef_df, target_term, title, is_split = TRUE) {
  plot_data <- coef_df %>% filter(term == target_term)
  if (nrow(plot_data) == 0) return(ggplot() + theme_void() + labs(title = paste("Missing data for", target_term)))
  
  p <- ggplot(plot_data, aes(x = date, y = estimate, color = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line(linewidth = 0.8) +
    theme_minimal() +
    labs(title = title, x = "Date", y = "Estimate", color = "Model")
  
  if (is_split) p <- p + facet_wrap(~ nuts1, scales = "free_y")
  return(p)
}

# 4. Coefficient Summary Plotter
plot_summary_statistics <- function(summary_table, terms_to_plot = NULL) {
  plot_data <- summary_table %>% filter(term != "(Intercept)")
  
  if (!is.null(terms_to_plot)) {
    plot_data <- plot_data %>% filter(term %in% terms_to_plot)
  }
  
  p <- ggplot(plot_data, aes(y = reorder(model, mean_estimate), x = mean_estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_linerange(aes(xmin = mean_estimate - sd_estimate, xmax = mean_estimate + sd_estimate), linewidth = 1.2, alpha = 0.6) +
    geom_point(size = 3.5) +
    geom_point(aes(x = median_estimate), size = 1.5, shape = 21, fill = "white", stroke = 1) +
    scale_x_continuous(n.breaks = 4) + 
    theme_minimal() +
    labs(
      title = "Coefficient Stability and Central Tendency",
      subtitle = "Solid dot = Mean | White dot = Median | Lines = ±1 SD",
      x = "Coefficient Estimate",
      y = "Model Specification"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10),
      axis.text.y = element_text(face = "bold", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1, "lines")
    )
  
  if ("nuts1" %in% colnames(plot_data) && length(unique(plot_data$nuts1)) > 1) {
    n_regions <- length(unique(plot_data$nuts1))
    p <- p + facet_wrap(~ term + nuts1, scales = "free_x", ncol = n_regions)
  } else {
    p <- p + facet_wrap(~ term, scales = "free_x", ncol = 2)
  }
  
  return(p)
}

# 5. Geographic Map Plotter
plot_coefficient_maps <- function(summary_table, shp_path, nuts_path, target_model = "SDM", target_term = "Spatial Lag (rho)") {
  nuts_data <- as.data.frame(fromJSON(nuts_path)$resultset) %>% 
    select(PRO_COM, contains("DEN")) %>% 
    rename(nuts1 = DEN_RIP, nuts2 = DEN_REG, nuts3 = DEN_UTS)
  
  shp_data <- st_read(shp_path, quiet = TRUE) %>%
    left_join(nuts_data, by = c("PRO_COM" = "PRO_COM"))
  
  region_polygons <- shp_data %>%
    group_by(nuts1) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  stats_subset <- summary_table %>% filter(model == target_model, term == target_term)
  map_data <- region_polygons %>% left_join(stats_subset, by = "nuts1")
  
  p_map <- ggplot(data = map_data) +
    geom_sf(aes(fill = mean_estimate), color = "white", linewidth = 0.3) +
    scale_fill_viridis_c(option = "magma", na.value = "grey80") +
    theme_void() +
    labs(
      title = sprintf("Geographic Distribution: %s", target_term),
      subtitle = sprintf("Model Specification: %s | Aggregation: NUTS1", target_model),
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
# EXECUTION BLOCK
# ------------------------------------------------------------------------------
parts_dir  <- "/Volumes/T7 Shield/FRES/fuels_data/results/model_parts"
output_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/standalone_analysis"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 1. Data Extraction
if(!file.exists(file.path(output_dir, "extracted_coefficients.csv"))) {
  stats_data <- analyze_standalone_results(parts_dir, split_by_nuts1 = TRUE)
  write_csv(stats_data, file.path(output_dir, "extracted_coefficients.csv"))
} else {
  stats_data <- read_csv(file.path(output_dir, "extracted_coefficients.csv"))
}


summary_table <- generate_summary_table(stats_data)
write_csv(summary_table, file.path(output_dir, "summary_statistics.csv"))

# 2. Coefficient Stability & Central Tendency Plot
key_terms <- c("Spatial Lag (rho)", "Time Lag (Gasoline)", "Time Lag (Brent Δ+)", "Time Lag (Brent Δ-)")
p_summary <- plot_summary_statistics(summary_table, terms_to_plot = key_terms)
ggsave(file.path(output_dir, "model_summaries.png"), plot = p_summary, width = 14, height = 10)

# 3. Time-Series Trajectory Plots
p_rho <- plot_comparative_timeseries(stats_data, "Spatial Lag (rho)", "Spatial Lag Over Time", is_split = TRUE)
ggsave(file.path(output_dir, "rho_timeseries.png"), plot = p_rho, width = 12, height = 8)

p_brent_pos <- plot_comparative_timeseries(stats_data, "Time Lag (Brent Δ+)", "Positive Brent Price Change Over Time", is_split = TRUE)
ggsave(file.path(output_dir, "brent_pos_timeseries.png"), plot = p_brent_pos, width = 12, height = 8)

p_brent_neg <- plot_comparative_timeseries(stats_data, "Time Lag (Brent Δ-)", "Negative Brent Price Change Over Time", is_split = TRUE)
ggsave(file.path(output_dir, "brent_neg_timeseries.png"), plot = p_brent_neg, width = 12, height = 8)

# 4. Geographic Map Plots
# Define geographic paths explicitly in the execution block
shp_path  <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"
nuts_path <- "/Volumes/T7 Shield/FRES/fuels_data/reportspooljson.json"

map_rho <- plot_coefficient_maps(summary_table, shp_path, nuts_path, target_model = "SDM", target_term = "Spatial Lag (rho)")
ggsave(file.path(output_dir, "map_spatial_lag_SDM.png"), plot = map_rho$plot, width = 10, height = 8)

map_brent_pos <- plot_coefficient_maps(summary_table, shp_path, nuts_path, target_model = "SDM", target_term = "Time Lag (Brent Δ+)")
ggsave(file.path(output_dir, "map_brent_pos_SDM.png"), plot = map_brent_pos$plot, width = 10, height = 8)

cat(sprintf("Diagnostic extraction and rendering complete. All visual assets saved to:\n%s\n", output_dir))

