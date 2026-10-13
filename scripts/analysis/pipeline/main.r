# ------------------------------------------------------------------------------
# PIPELINE WRAPPER AND EXECUTION SCRIPT
# ------------------------------------------------------------------------------
base_path <- "/Volumes/T7 Shield/FRES/repos/fuels_paper_sie"
setwd(base_path)

source("scripts/analysis/pipeline/data_preparation.r")
source("scripts/analysis/pipeline/estimation.r")
source("scripts/analysis/pipeline/analyze_results.r")

library(tidyverse)
library(sf)
library(jsonlite) 

# --- CENTRAL MODEL CONFIGURATION ---
pipeline_config <- list(
  dependent_var = "mean_price_gasoline_self",
  base_features = c(
    "population_density_km2"
  ),
  conditional_features = c("brand_group", "station_type"),
  term_labels = c(
    "(Intercept)"                      = "Intercept",
    "population_density_km2"           = "Population Density",
    "brand_groupminor"                 = "Minor Brand",
    "station_typeAltro"                = "Other Station Type",
    "lambda"                           = "Spatial Lag (rho)",
    "rho"                              = "Spatial Lag (rho)"
  ),
  spatial_models = list(
    SLX   = list(pkg = "plm",  lag = FALSE, spatial.error = FALSE, Durbin = TRUE),
    SEM   = list(pkg = "splm", lag = FALSE, spatial.error = TRUE,  Durbin = FALSE),
    SDEM  = list(pkg = "splm", lag = FALSE, spatial.error = TRUE,  Durbin = TRUE),
    SAR   = list(pkg = "splm", lag = TRUE,  spatial.error = FALSE, Durbin = FALSE),
    SDM   = list(pkg = "splm", lag = TRUE,  spatial.error = FALSE, Durbin = TRUE),
    SARAR = list(pkg = "splm", lag = TRUE,  spatial.error = TRUE,  Durbin = FALSE),
    GNS   = list(pkg = "splm", lag = TRUE,  spatial.error = TRUE,  Durbin = TRUE)
  )
)

run_spatial_pipeline <- function(rdata_dir, stations_csv, pop_dir, shp_path, nuts_path, mappings_dir, output_dir, 
                                 config, window_weeks = 52, radius_m = 30000, 
                                 scope_level = "all", scope_name = NULL, estimation_granularity = "nuts1",
                                 start_week = NULL, end_week = NULL,
                                 min_stations = 30, sample_fraction = 0.10, test = FALSE, seed = 42) {
  
  set.seed(seed)
  
  # --- 1. VALIDATION & SETUP BLOCK ---
  valid_levels <- c("all", "nuts1", "nuts2", "nuts3")
  if (!scope_level %in% valid_levels) stop("Error: scope_level must be 'all', 'nuts1', 'nuts2', or 'nuts3'.")
  if (scope_level != "all" && is.null(scope_name)) stop(sprintf("Error: You selected scope level '%s' but did not provide a scope_name.", scope_level))
  if (!is.null(scope_name) && length(scope_name) > 1) stop("Error: Please select strictly ONE scope name.")
  if (!estimation_granularity %in% c("nuts1", "nuts2", "nuts3")) stop("Error: estimation_granularity must be 'nuts1', 'nuts2', or 'nuts3'.")
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Combine model config and execution parameters
  run_metadata <- list(
    model_config = config,
    execution_params = list(
      window_weeks = window_weeks,
      radius_m = radius_m,
      min_stations = min_stations,
      sample_fraction = sample_fraction,
      test_mode = test,
      random_seed = seed,
      scope_level = scope_level,
      scope_name = scope_name,
      estimation_granularity = estimation_granularity,
      start_week = start_week,
      end_week = end_week
    )
  )
  
  # Save the complete metadata to the run directory
  saveRDS(run_metadata, file.path(output_dir, "run_metadata.RDS"))
  write_json(run_metadata, file.path(output_dir, "run_metadata.json"), pretty = TRUE, auto_unbox = TRUE)
  
  # Save the pipeline configuration to the run directory
  saveRDS(config, file.path(output_dir, "pipeline_config.RDS"))
  write_json(config, file.path(output_dir, "pipeline_config.json"), pretty = TRUE, auto_unbox = TRUE)
  
  # --- 2. DATA PREPARATION ---
  cat("\n=== PHASE 1: Data Preparation & Stratified Sampling ===\n")
  base_data <- prepare_integrated_panel(
    rdata_dir = rdata_dir, stations_list_file = stations_csv, pop_dir = pop_dir, shp_path = shp_path,
    mappings_dir = mappings_dir, nuts_path = nuts_path, min_stations = min_stations,
    sample_fraction = sample_fraction, test = test, scope_level = scope_level, scope_name = scope_name, seed = seed
  )
  
  if (!is.null(start_week) && !is.null(end_week)) {
    cat(sprintf("Filtering data for timeframe: %s to %s\n", start_week, end_week))
    base_data$df <- base_data$df %>% 
      filter(time_index >= start_week & time_index <= end_week)
  }
  
  saveRDS(base_data$df, file.path(output_dir, "sampled_panel_data.RDS"))
  st_write(base_data$stations_sf, file.path(output_dir, "sampled_stations.gpkg"), append = FALSE, quiet = TRUE)
  if (length(base_data$failed_files) > 0) writeLines(base_data$failed_files, file.path(output_dir, "failed_rds_files.txt"))
  
  # --- 3. ROLLING WINDOW ESTIMATION ---
  cat("\n=== PHASE 2: Rolling Panel Models ===\n")
  rolling_output <- run_rolling_panel_models(
    base_data = base_data, config = config, output_dir = output_dir, window_size = window_weeks,
    radius_m = radius_m, estimation_granularity = estimation_granularity, seed = seed
  )
  saveRDS(rolling_output, file.path(output_dir, "rolling_models_metadata.RDS"))
  
  # --- 4. DIAGNOSTICS & ANALYSIS ---
  cat("\n=== PHASE 3: Diagnostics & Analysis ===\n")
  parts_dir <- file.path(output_dir, "model_parts")
  stats_data <- analyze_standalone_results(parts_dir, config = config, is_split = TRUE)
  write_csv(stats_data, file.path(output_dir, "comparative_model_stats.csv"))
  
  summary_stats <- generate_summary_table(stats_data)
  write_csv(summary_stats, file.path(output_dir, "summary_statistics.csv"))
  
  # --- 5. VISUALIZATIONS & PRESENTATION TABLES ---
  cat("\n=== PHASE 4: Generating Visualizations & Tables ===\n")
  export_compact_tables(summary_stats, output_dir)
  generate_all_visualizations(
    stats_data = stats_data, 
    summary_stats = summary_stats, 
    output_dir = output_dir, 
    shp_path = shp_path, 
    nuts_path = nuts_path, 
    estimation_granularity = estimation_granularity
  )
  
  cat("\nPipeline Execution Complete! Outputs saved to:", output_dir, "\n")
  return(list(rolling_metadata = rolling_output, stats_data = stats_data, summary_stats = summary_stats))
}

# ------------------------------------------------------------------------------
# EXECUTION BLOCK 
# ------------------------------------------------------------------------------
rdata_dir       <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly"
stations_csv    <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
pop_dir         <- "/Volumes/T7 Shield/FRES/fuels_data/population"
shp_path        <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"
nuts_path       <- "/Volumes/T7 Shield/FRES/fuels_data/reportspooljson.json"
mappings_dir    <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly" 
base_output_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results"

window_weeks <- 52; radius_m <- 30000; min_stations <- 30; sample_fraction <- 0.05
test_mode <- FALSE; random_seed <- 42
scope_level <- "nuts2"; scope_name <- "Lombardia"; estimation_granularity <- "nuts3"

start_week <- "2022-01" 
end_week   <- "2023-52"

timestamp_str <- format(Sys.time(), "%Y%m%d%H%M")
region_prefix <- ifelse(scope_level == "all", "All_Regions", scope_name)
run_output_dir <- file.path(base_output_dir, paste0(region_prefix, "_", timestamp_str))

if (!dir.exists(run_output_dir)) dir.create(run_output_dir, recursive = TRUE)



pipeline_results <- run_spatial_pipeline(
  rdata_dir = rdata_dir, stations_csv = stations_csv, pop_dir = pop_dir, shp_path = shp_path,
  nuts_path = nuts_path, mappings_dir = mappings_dir, output_dir = run_output_dir, config = pipeline_config,
  window_weeks = window_weeks, radius_m = radius_m, scope_level = scope_level, scope_name = scope_name,
  estimation_granularity = estimation_granularity, 
  start_week = start_week, end_week = end_week, 
  min_stations = min_stations, sample_fraction = sample_fraction, test = test_mode, seed = random_seed
)