# ------------------------------------------------------------------------------
# SENSITIVITY ANALYSIS: SPATIAL RADIUS AND NEIGHBOR DISTRIBUTION
# ------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(spdep)
library(splm)
library(plm)
library(ggplot2)
library(patchwork) # For combining plots

# 1. FUNCTION: ANALYZE NEIGHBORS AND ESTIMATE MODEL PER RADIUS
# ------------------------------------------------------------------------------
run_radius_sensitivity <- function(base_data, radii_test, test_window_weeks = 52, target_model = "SDM") {
  
  df_all <- base_data$df
  stations_sf <- base_data$stations_sf
  
  # Select a single time window for the sensitivity test to minimize execution time
  unique_weeks <- sort(unique(df_all$time_index))
  window_weeks <- unique_weeks[1:test_window_weeks]
  
  # Filter balanced panel for this window
  complete_pumps <- df_all %>%
    filter(time_index %in% window_weeks) %>%
    filter(complete.cases(mean_price_gasoline_self, latitude, longitude, 
                          population_density_km2, brand_group, station_type)) %>%
    count(id_pump) %>% 
    filter(n == test_window_weeks) %>% 
    pull(id_pump)
  
  df_window <- df_all %>% filter(time_index %in% window_weeks, id_pump %in% complete_pumps) %>% arrange(id_pump)
  stations_window_sf <- stations_sf %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump)
  coords <- st_coordinates(stations_window_sf)
  
  base_formula <- mean_price_gasoline_self ~ lag(mean_price_gasoline_self, 1) + 
    lag(brent_price_mean, 1) + population_density_km2 + brand_group + station_type
  
  results_list <- list()
  neighbor_stats_list <- list()
  
  for (r in radii_test) {
    cat(sprintf("Testing Radius: %d meters...\n", r))
    
    # 1. Neighbor Distribution Analysis
    nb <- dnearneigh(coords, d1 = 0, d2 = r)
    n_counts <- card(nb)
    isolated_idx <- which(n_counts == 0)
    
    # Store neighbor distribution data
    neighbor_stats_list[[as.character(r)]] <- data.frame(
      radius = r,
      id_pump = stations_window_sf$id_pump,
      neighbors = n_counts
    )
    
    # 2. Model Estimation (handling isolated stations)
    if (length(isolated_idx) > 0) {
      stat_win_active <- stations_window_sf[-isolated_idx, ]
      df_win_active <- df_window %>% filter(id_pump %in% stat_win_active$id_pump)
      coords_active <- st_coordinates(stat_win_active)
      nb_active <- dnearneigh(coords_active, d1 = 0, d2 = r)
    } else {
      df_win_active <- df_window
      coords_active <- coords
      nb_active <- nb
    }
    
    W <- nb2listw(nb_active, glist = lapply(nbdists(nb_active, coords_active), function(x) ifelse(x>0, 1/x, 0)), 
                  style = "W", zero.policy = TRUE)
    df_panel <- pdata.frame(df_win_active, index = c("id_pump", "time_index"))
    
    # Configure model based on target_model
    is_durbin <- target_model %in% c("SDM", "SDEM", "GNS")
    has_error <- target_model %in% c("SEM", "SDEM", "SARAR", "GNS")
    has_lag   <- target_model %in% c("SAR", "SDM", "SARAR", "GNS")
    
    tryCatch({
      model <- spgm(
        base_formula, data = df_panel, listw = W, 
        lag = has_lag, spatial.error = has_error, Durbin = is_durbin, method = "w2sls"
      )
      
      summ <- summary(model)
      coefs <- as.data.frame(summ$CoefTable)
      
      # Extract Spatial Lag (rho) or Spatial Error (lambda)
      spatial_coef <- if("lambda" %in% rownames(coefs)) coefs["lambda", ] else coefs[grep("rho|lambda", rownames(coefs))[1], ]
      
      results_list[[as.character(r)]] <- data.frame(
        radius = r,
        estimate = spatial_coef[1, 1],
        std_error = spatial_coef[1, 2],
        p_value = spatial_coef[1, 4],
        n_stations_active = nrow(stat_win_active),
        pct_isolated = (length(isolated_idx) / nrow(stations_window_sf)) * 100
      )
    }, error = function(e) {
      cat(sprintf("  -> Model failed for radius %d: %s\n", r, e$message))
    })
  }
  
  return(list(
    models_summary = bind_rows(results_list),
    neighbors_dist = bind_rows(neighbor_stats_list)
  ))
}

# 2. FUNCTION: VISUALIZE SENSITIVITY RESULTS
# ------------------------------------------------------------------------------
plot_sensitivity_analysis <- function(sensitivity_results) {
  df_models <- sensitivity_results$models_summary
  df_neighbors <- sensitivity_results$neighbors_dist
  
  df_neighbors$radius_f <- factor(df_neighbors$radius)
  
  # Plot A: Neighbor Distribution (Boxplot)
  p_neighbors <- ggplot(df_neighbors, aes(x = radius_f, y = neighbors)) +
    geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.size = 0.5) +
    theme_minimal() +
    labs(
      title = "A. Distribution of Neighbors per Station",
      x = "Search Radius (meters)",
      y = "Number of Neighbors"
    )
  
  # Plot B: Spatial Coefficient Estimate vs Radius
  p_coef <- ggplot(df_models, aes(x = radius, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line(color = "darkred", linewidth = 1) +
    geom_point(color = "darkred", size = 3) +
    geom_ribbon(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error), 
                alpha = 0.2, fill = "darkred") +
    scale_x_continuous(breaks = df_models$radius) +
    theme_minimal() +
    labs(
      title = "B. Spatial Lag Coefficient Sensitivity",
      x = "Search Radius (meters)",
      y = "Coefficient Estimate (95% CI)"
    )
  
  # Plot C: Percentage of Isolated Stations
  p_isolated <- ggplot(df_models, aes(x = radius, y = pct_isolated)) +
    geom_line(color = "darkorange", linewidth = 1) +
    geom_point(color = "darkorange", size = 3) +
    scale_x_continuous(breaks = df_models$radius) +
    theme_minimal() +
    labs(
      title = "C. Isolated Stations (Dropped from W Matrix)",
      x = "Search Radius (meters)",
      y = "Isolated Stations (%)"
    )
  
  # Combine plots using patchwork
  combined_plot <- (p_neighbors | p_isolated) / p_coef + 
    plot_annotation(title = "Spatial Weight Matrix (W) Sensitivity Analysis")
  
  print(combined_plot)
  return(combined_plot)
}

# 3. EXECUTION SCRIPT
# ------------------------------------------------------------------------------
# Define input paths
df_file      <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/part_0.RDS"
rdata_dir    <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly"
stations_csv <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
pop_dir      <- "/Volumes/T7 Shield/FRES/fuels_data/population"
shp_path     <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"

output_dir   <- "/Volumes/T7 Shield/FRES/fuels_data/results/sensitivity"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("Generating base_data...\n")
# NOTE: Ensure the prepare_integrated_panel() function (and its helper functions) 
# are loaded in your environment or sourced from your main pipeline script.
base_data <- prepare_integrated_panel(
  data_file          = df_file,
  stations_list_file = stations_csv,
  pop_dir            = pop_dir,
  shp_path           = shp_path,
  mappings_dir       = rdata_dir
)

radii_to_test <- c(5000, 10000, 15000, 20000, 30000, 50000)

cat("Starting Radius Sensitivity Analysis...\n")
sensitivity_results <- run_radius_sensitivity(
  base_data = base_data, 
  radii_test = radii_to_test, 
  test_window_weeks = 52, 
  target_model = "SDM"
)

cat("Generating Plots...\n")
p_sensitivity <- plot_sensitivity_analysis(sensitivity_results)

# Save Outputs
write_csv(sensitivity_results$models_summary, file.path(output_dir, "radius_sensitivity_stats.csv"))
write_csv(sensitivity_results$neighbors_dist, file.path(output_dir, "neighbors_distribution.csv"))
ggsave(file.path(output_dir, "radius_sensitivity_plots.png"), plot = p_sensitivity, width = 12, height = 10)

cat("Sensitivity analysis complete. Results saved to:", output_dir, "\n")