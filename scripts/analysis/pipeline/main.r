base_path <- "/Volumes/T7 Shield/FRES/repos/fuels_paper_sie"

setwd(base_path)

source("scripts/analysis/pipeline/data_preparation.r")
source("scripts/analysis/pipeline/estimation.r")
source("scripts/analysis/pipeline/analyze_results.r")

run_spatial_pipeline <- function(rdata_dir, stations_csv, pop_dir, shp_path, nuts_path, mappings_dir, output_dir, 
                                 window_weeks = 52, radius_m = 30000, split_by_nuts1 = FALSE,
                                 min_stations = 30, sample_fraction = 0.10, test = FALSE) {
  
  cat("Starting Data Preparation and Stratified Sampling...\n")
  base_data <- prepare_integrated_panel(
    rdata_dir          = rdata_dir,
    stations_list_file = stations_csv,
    pop_dir            = pop_dir,
    shp_path           = shp_path,
    mappings_dir       = mappings_dir,
    nuts_path          = nuts_path,
    min_stations       = min_stations,
    sample_fraction    = sample_fraction,
    test               = test
  )
  cat("Data Preparation Complete.\n\n")
  
  cat(sprintf("Starting Rolling Panel Models (%d-week windows)...\n", window_weeks))
  rolling_output <- run_rolling_panel_models(
    base_data      = base_data,
    output_dir     = output_dir, # Added parameter
    window_size    = window_weeks,
    radius_m       = radius_m,
    split_by_nuts1 = split_by_nuts1
  )
  cat("Model Execution Complete.\n\n")
  
  cat("Starting Diagnostics and Analysis...\n")
  cat("Analysis Complete.\n\n")
  
  # Save Outputs
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Save sampled panel and stations to avoid re-running sampling next time
  saveRDS(base_data$df, file.path(output_dir, "sampled_panel_data.RDS"))
  st_write(base_data$stations_sf, file.path(output_dir, "sampled_stations.gpkg"), append = FALSE, quiet = TRUE)
  
  if (length(base_data$failed_files) > 0) {
    writeLines(base_data$failed_files, file.path(output_dir, "failed_rds_files.txt"))
  }
  
  saveRDS(rolling_output, file.path(output_dir, "rolling_models_output.RDS"))
  write_csv(diagnostics$stats_data, file.path(output_dir, "comparative_model_stats.csv"))
  
  ggsave(file.path(output_dir, "spatial_lag_comparison.png"), plot = diagnostics$plots[[1]], width = 12, height = 8)
  ggsave(file.path(output_dir, "brent_lag_comparison.png"), plot = diagnostics$plots[[2]], width = 12, height = 8)
  
  # Summary statistics and plots
  summary_stats <- generate_summary_table(diagnostics$stats_data)
  write_csv(summary_stats, file.path(output_dir, "summary_statistics.csv"))
  
  key_terms <- c("Spatial Lag (rho)", "Time Lag (Gasoline)", "Time Lag (Brent Δ+)", "Time Lag (Brent Δ-)")
  p_summary <- plot_summary_statistics(summary_stats, terms_to_plot = key_terms)
  ggsave(file.path(output_dir, "model_summaries.png"), plot = p_summary, width = 14, height = 10)
  
  # Geographic mapping if spatial split is activated
  if (split_by_nuts1) {
    cat("Generating Geographic Maps...\n")
    map_rho <- plot_coefficient_maps(summary_stats, shp_path, nuts_path, 
                                     target_model = "SDM", target_term = "Spatial Lag (rho)")
    map_brent_pos <- plot_coefficient_maps(summary_stats, shp_path, nuts_path, 
                                           target_model = "SDM", target_term = "Time Lag (Brent Δ+)")
    
    ggsave(file.path(output_dir, "map_spatial_lag_SDM.png"), plot = map_rho$plot, width = 10, height = 8)
    ggsave(file.path(output_dir, "map_brent_pos_SDM.png"), plot = map_brent_pos$plot, width = 10, height = 8)
  }
  
  cat("Outputs saved to:", output_dir, "\n")
  
  return(list(
    rolling_results = rolling_output,
    diagnostics = diagnostics,
    summary_stats = summary_stats
  ))
}

# ------------------------------------------------------------------------------
# 7. EXECUTION SCRIPT
# ------------------------------------------------------------------------------

# Define Input and Output Paths
rdata_dir    <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly"
stations_csv <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
pop_dir      <- "/Volumes/T7 Shield/FRES/fuels_data/population"
shp_path     <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"
nuts_path    <- "/Volumes/T7 Shield/FRES/fuels_data/reportspooljson.json"
mappings_dir <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly" 
output_dir   <- "/Volumes/T7 Shield/FRES/fuels_data/results"

# Define Execution Parameters
window_weeks    <- 52
radius_m        <- 30000
split_by_nuts1  <- TRUE # Set to TRUE to run regional analysis and generate maps
min_stations    <- 30
sample_fraction <- 0.10
test_mode       <- FALSE

# Execute Pipeline
pipeline_results <- run_spatial_pipeline(
  rdata_dir       = rdata_dir,
  stations_csv    = stations_csv,
  pop_dir         = pop_dir,
  shp_path        = shp_path,
  nuts_path       = nuts_path,
  mappings_dir    = mappings_dir,
  output_dir      = output_dir,
  window_weeks    = window_weeks,
  radius_m        = radius_m,
  split_by_nuts1  = split_by_nuts1,
  min_stations    = min_stations,
  sample_fraction = sample_fraction,
  test            = test_mode
)
