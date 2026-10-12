# ------------------------------------------------------------------------------
# PIPELINE WRAPPER AND EXECUTION SCRIPT (SYNCHRONIZED)
# ------------------------------------------------------------------------------
base_path <- "/Volumes/T7 Shield/FRES/repos/fuels_paper_sie"
setwd(base_path)

source("scripts/analysis/pipeline/data_preparation.r")
source("scripts/analysis/pipeline/estimation.r")
source("scripts/analysis/pipeline/analyze_results.r")

library(tidyverse)
library(sf)

run_spatial_pipeline <- function(rdata_dir, stations_csv, pop_dir, shp_path, nuts_path, mappings_dir, output_dir, 
                                 window_weeks = 52, radius_m = 30000, 
                                 scope_level = "all", scope_name = NULL, estimation_granularity = "nuts1",
                                 min_stations = 30, sample_fraction = 0.10, test = FALSE, seed = 42) {
  
  set.seed(seed)
  
  # --- 1. VALIDATION BLOCK ---
  valid_levels <- c("all", "nuts1", "nuts2", "nuts3")
  if (!scope_level %in% valid_levels) {
    stop("Error: scope_level must be 'all', 'nuts1', 'nuts2', or 'nuts3'.")
  }
  if (scope_level != "all" && is.null(scope_name)) {
    stop(sprintf("Error: You selected scope level '%s' but did not provide a scope_name.", scope_level))
  }
  if (!is.null(scope_name) && length(scope_name) > 1) {
    stop("Error: Please select strictly ONE scope name to avoid memory bugs and overlapping runs.")
  }
  if (!estimation_granularity %in% c("nuts1", "nuts2", "nuts3")) {
    stop("Error: estimation_granularity must be 'nuts1', 'nuts2', or 'nuts3'.")
  }
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # --- 2. DATA PREPARATION ---
  cat("\n=== PHASE 1: Data Preparation & Stratified Sampling ===\n")
  base_data <- prepare_integrated_panel(
    rdata_dir          = rdata_dir,
    stations_list_file = stations_csv,
    pop_dir            = pop_dir,
    shp_path           = shp_path,
    mappings_dir       = mappings_dir,
    nuts_path          = nuts_path,
    min_stations       = min_stations,
    sample_fraction    = sample_fraction,
    test               = test,
    scope_level        = scope_level,
    scope_name         = scope_name,
    seed               = seed
  )
  
  # Save baseline data
  saveRDS(base_data$df, file.path(output_dir, "sampled_panel_data.RDS"))
  st_write(base_data$stations_sf, file.path(output_dir, "sampled_stations.gpkg"), append = FALSE, quiet = TRUE)
  if (length(base_data$failed_files) > 0) {
    writeLines(base_data$failed_files, file.path(output_dir, "failed_rds_files.txt"))
  }
  
  # --- 3. ROLLING WINDOW ESTIMATION ---
  cat("\n=== PHASE 2: Rolling Panel Models ===\n")
  rolling_output <- run_rolling_panel_models(
    base_data              = base_data,
    output_dir             = output_dir,
    window_size            = window_weeks,
    radius_m               = radius_m,
    estimation_granularity = estimation_granularity,
    seed                   = seed
  )
  saveRDS(rolling_output, file.path(output_dir, "rolling_models_metadata.RDS"))
  
  # --- 4. DIAGNOSTICS & ANALYSIS ---
  cat("\n=== PHASE 3: Diagnostics & Analysis ===\n")
  # Read dynamically from the generated parts directory
  parts_dir <- file.path(output_dir, "model_parts")
  stats_data <- analyze_standalone_results(parts_dir, is_split = TRUE)
  write_csv(stats_data, file.path(output_dir, "comparative_model_stats.csv"))
  
  # Generate summaries
  summary_stats <- generate_summary_table(stats_data)
  write_csv(summary_stats, file.path(output_dir, "summary_statistics.csv"))
  
  # --- 5. VISUALIZATION EXPORTS ---
  cat("\n=== PHASE 4: Generating Visualizations ===\n")
  
  # A. Coefficient Summaries
  key_terms <- c("Spatial Lag (rho)", "Time Lag (Gasoline)", "Time Lag (Brent Δ+)", "Time Lag (Brent Δ-)")
  p_summary <- plot_summary_statistics(summary_stats, terms_to_plot = key_terms)
  ggsave(file.path(output_dir, "model_summaries.png"), plot = p_summary, width = 14, height = 10)
  
  # B. Time-Series Trajectories
  p_rho <- plot_comparative_timeseries(stats_data, "Spatial Lag (rho)", "Spatial Lag Over Time", is_split = TRUE)
  ggsave(file.path(output_dir, "rho_timeseries.png"), plot = p_rho, width = 12, height = 8)
  
  p_brent_pos <- plot_comparative_timeseries(stats_data, "Time Lag (Brent Δ+)", "Positive Brent Price Change", is_split = TRUE)
  ggsave(file.path(output_dir, "brent_pos_timeseries.png"), plot = p_brent_pos, width = 12, height = 8)
  
  # C. Geographic Mapping
  cat("Generating Geographic Maps for", toupper(estimation_granularity), "...\n")
  
  map_rho <- plot_coefficient_maps(summary_stats, shp_path, nuts_path, 
                                   target_nuts_level = estimation_granularity, 
                                   target_model = "SDM", target_term = "Spatial Lag (rho)")
  ggsave(file.path(output_dir, sprintf("map_spatial_lag_SDM_%s.png", estimation_granularity)), 
         plot = map_rho$plot, width = 10, height = 8)
  
  map_brent_pos <- plot_coefficient_maps(summary_stats, shp_path, nuts_path, 
                                         target_nuts_level = estimation_granularity, 
                                         target_model = "SDM", target_term = "Time Lag (Brent Δ+)")
  ggsave(file.path(output_dir, sprintf("map_brent_pos_SDM_%s.png", estimation_granularity)), 
         plot = map_brent_pos$plot, width = 10, height = 8)
  
  cat("\nPipeline Execution Complete! Outputs saved to:", output_dir, "\n")
  
  return(list(
    rolling_metadata = rolling_output,
    stats_data = stats_data,
    summary_stats = summary_stats
  ))
}

# ------------------------------------------------------------------------------
# 7. EXECUTION BLOCK (WITH DYNAMIC DIRECTORY & CONFIG LOGGING)
# ------------------------------------------------------------------------------

# Define Input and Base Output Paths
rdata_dir       <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly"
stations_csv    <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
pop_dir         <- "/Volumes/T7 Shield/FRES/fuels_data/population"
shp_path        <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"
nuts_path       <- "/Volumes/T7 Shield/FRES/fuels_data/reportspooljson.json"
mappings_dir    <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly" 
base_output_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results"

# Define Execution Parameters
window_weeks       <- 52
radius_m           <- 30000
min_stations       <- 30
sample_fraction    <- 0.05
test_mode          <- FALSE
random_seed        <- 42

# --- SCOPE AND GRANULARITY SELECTION ---
scope_level            <- "nuts2" 
scope_name             <- "Lombardia" 
estimation_granularity <- "nuts3"
# ---------------------------------------

# --- DYNAMIC OUTPUT DIRECTORY CREATION ---
timestamp_str <- format(Sys.time(), "%Y%m%d%H%M")
region_prefix <- ifelse(scope_level == "all", "All_Regions", scope_name)
run_output_dir <- file.path(base_output_dir, paste0(region_prefix, "_", timestamp_str))

if (!dir.exists(run_output_dir)) dir.create(run_output_dir, recursive = TRUE)
cat(sprintf("Created specific output directory:\n%s\n", run_output_dir))

# --- SAVE RUN CONFIGURATION LOG ---
config_log_path <- file.path(run_output_dir, "run_config.log")
config_details <- c(
  sprintf("Random Seed:            %d", random_seed),
  "=== SPATIAL PIPELINE RUN CONFIGURATION ===",
  sprintf("Timestamp:              %s", timestamp_str),
  sprintf("Scope Level:            %s", scope_level),
  sprintf("Scope Name:             %s", ifelse(is.null(scope_name), "None", scope_name)),
  sprintf("Estimation Granularity: %s", estimation_granularity),
  sprintf("Window Weeks:           %d", window_weeks),
  sprintf("Radius (m):             %d", radius_m),
  sprintf("Min Stations:           %d", min_stations),
  sprintf("Sample Fraction:        %.2f", sample_fraction),
  sprintf("Test Mode:              %s", as.character(test_mode)),
  "",
  "=== DIRECTORIES ===",
  sprintf("Output Directory:       %s", run_output_dir),
  sprintf("RData Directory:        %s", rdata_dir),
  sprintf("Stations Data:          %s", stations_csv),
  sprintf("Population Data:        %s", pop_dir),
  sprintf("Shapefile Path:         %s", shp_path),
  sprintf("NUTS Mappings:          %s", nuts_path)
)
writeLines(config_details, config_log_path)
cat("Run configuration saved to log.\n")
# -----------------------------------------

# Execute Pipeline
pipeline_results <- run_spatial_pipeline(
  rdata_dir              = rdata_dir,
  stations_csv           = stations_csv,
  pop_dir                = pop_dir,
  shp_path               = shp_path,
  nuts_path              = nuts_path,
  mappings_dir           = mappings_dir,
  output_dir             = run_output_dir,
  window_weeks           = window_weeks,
  radius_m               = radius_m,
  scope_level            = scope_level,
  scope_name             = scope_name,
  estimation_granularity = estimation_granularity,
  min_stations           = min_stations,
  sample_fraction        = sample_fraction,
  test                   = test_mode,
  seed                   = random_seed
)