# ------------------------------------------------------------------------------
# setup_pipeline.r (UPDATED FOR CENTRALIZED LOGGING)
# ------------------------------------------------------------------------------
setup_pipeline <- function(data_dir, output_dir, estimation_granularity, random_seed, test = FALSE, run_dir = NULL) {
  
  log_msg("Initializing pipeline setup...")
  
  if(isTRUE(test)) {
    base_data_path <- file.path(data_dir, "dataset_test.RDS")
    log_msg("Test mode active: Using dataset_test.RDS")
  } else {
    base_data_path <- file.path(data_dir, "dataset.RDS")
  }
  
  geo_data_dir <- file.path(data_dir, "geo_data")
  tmstp <- format(Sys.time(), "%Y%m%d_%H%M%S") 
  run_name <- paste("run", estimation_granularity, random_seed, tmstp, sep = "_")
  
  if(!is.null(run_dir) && !dir.exists(run_dir)) {
    stop("Specified run directory does not exist: ", run_dir)
  }
  
  if(!is.null(run_dir) && dir.exists(run_dir)) {
    log_msg("Running pipeline in specified directory: ", run_dir)
  }
  
  if(is.null(run_dir)) {
    run_dir <- file.path(output_dir, run_name)
  }
  
  results_file_path <- file.path(run_dir, paste0("results_", run_name, ".RDS"))
  
  if(!dir.exists(run_dir)) {
    dir.create(run_dir, recursive = TRUE)
    log_msg("Created new run directory: ", run_dir)
  }
  
  geo_data_path <- file.path(geo_data_dir, "geo_data.RDS")
  shp_path <- file.path(geo_data_dir, "city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp")
  nuts_path <- file.path(geo_data_dir, "nuts_data.csv")
  
  if(!file.exists(geo_data_path)) {
    log_msg("geo_data.RDS not found. Merging shapefile and NUTS data...")
    merge_shp_nuts(nuts_path, shp_path, geo_data_path) 
    log_msg("Geospatial data successfully merged and saved.")
  } else {
    log_msg("Existing geo_data.RDS found.")
  }
  
  log_msg("Setup complete.")
  
  return(
    list(
      run_dir = run_dir,
      base_data_path = base_data_path, 
      geo_data_path = geo_data_path,
      results_file_path = results_file_path
    )
  )
}
