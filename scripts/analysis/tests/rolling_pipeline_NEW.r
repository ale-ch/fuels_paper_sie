library(foreach)
library(doParallel)
library(dplyr)
library(plm)
library(splm)
library(spdep)
library(sf)

source("/Volumes/T7 Shield/FRES/repos/fuels_paper_sie/scripts/utils/create_coefs_df.r")

# ------------------------------------------------------------------------------
# 0. LOGGING UTILITY
# ------------------------------------------------------------------------------
log_file_path <- NULL

setup_logging <- function(output_dir, config) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  log_file_path <<- file.path(output_dir, paste0("execution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  file.create(log_file_path)
  
  log_msg("=== PIPELINE INITIALIZED ===")
  log_msg("Output directory: ", output_dir)
  log_msg("Configuration Setup:")
  
  # Dump config safely to the log file
  config_text <- paste(capture.output(print(config)), collapse = "\n")
  cat(config_text, "\n\n", file = log_file_path, append = TRUE)
}

log_msg <- function(...) {
  msg <- sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...))
  cat(msg)
  if (!is.null(log_file_path) && file.exists(log_file_path)) {
    cat(msg, file = log_file_path, append = TRUE)
  }
}
# ------------------------------------------------------------------------------
# 1. CENTRAL MODEL CONFIGURATION
# ------------------------------------------------------------------------------
config <- list(
  dependent_var = "mean_price_gasoline_self",
  base_features = c("population_density_km2", "brent_diff_pos", "brent_diff_neg"),
  term_labels = c(
    "(Intercept)"    = "Intercept",
    "lambda"         = "Spatial Lag (lambda)",
    "rho"            = "Spatial Error (rho)"
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

# ------------------------------------------------------------------------------
# 2. MODULAR PIPELINE FUNCTIONS
# ------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(spdep)
library(plm)
library(splm)

apply_stratified_sampling <- function(df, granularity, sample_fraction, random_seed = 42) {
  
  set.seed(random_seed)
  if (sample_fraction >= 1.0) return(df)
  
  log_msg("Applying stratified sampling (", sample_fraction * 100, "%) at ", granularity, " level.")
  
  # Group by granularity, pull a fraction of unique pumps
  sampled_pumps <- df %>%
    group_by(id_pump) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(!!sym(granularity)) %>%
    slice_sample(prop = sample_fraction) %>%
    pull(id_pump)
  
  result_df <- df %>% filter(id_pump %in% sampled_pumps)
  log_msg("Retained ", length(unique(result_df$id_pump)), " unique stations for estimation.")
  
  return(result_df)
}

prepare_window_data <- function(df, start_week, window_size) {
  end_week <- start_week + window_size - 1
  
  window_df <- df %>% filter(time_index >= start_week, time_index <= end_week)
  if (nrow(window_df) == 0) {
    log_msg("    -> SKIP: 0 rows found in time_index range [", start_week, " - ", end_week, "]")
    return(NULL)
  }
  
  complete_pumps <- window_df %>%
    filter(
      complete.cases(mean_price_gasoline_self, latitude, longitude),
      !is.nan(mean_price_gasoline_self)
    ) %>%
    count(id_pump) %>% 
    filter(n == window_size) %>% 
    pull(id_pump)
  
  if (length(complete_pumps) < 10) {
    log_msg("    -> SKIP: Only ", length(complete_pumps), " strictly balanced stations (need >=10).")
    return(NULL)
  }
  
  window_df <- window_df %>% 
    filter(id_pump %in% complete_pumps) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
    st_transform(32632)
  
  window_df_unique <- window_df %>% 
    group_by(id_pump) %>% 
    slice(1) %>% 
    ungroup()
  
  log_msg("    -> SUCCESS: Window balanced with ", length(complete_pumps), " active stations.")
  return(list(df = window_df, df_unique = window_df_unique))
}

compute_distance_matrix <- function(window_data, radius_m) {
  df <- window_data$df
  df_unique <- window_data$df_unique
  
  coords <- st_coordinates(df_unique)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
  
  isolated <- which(card(nb) == 0)
  if (length(isolated) > 0) {
    log_msg("    -> MATRIX: Dropping ", length(isolated), " isolated stations with 0 neighbors.")
    df_unique <- df_unique[-isolated, ]
  }
  
  if (nrow(df_unique) < 10) {
    log_msg("    -> SKIP: Matrix too small after dropping isolates (", nrow(df_unique), " remaining).")
    return(NULL)
  }
  
  df <- df %>% filter(id_pump %in% df_unique$id_pump) %>% arrange(id_pump, time_index)
  df_unique <- df_unique %>% arrange(id_pump)
  
  coords <- st_coordinates(df_unique)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
  
  W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
                style = "W", zero.policy = TRUE)
  
  log_msg("    -> SUCCESS: Spatial matrix W created (N = ", nrow(df_unique), ").")
  return(list(df = df, W = W))
}

fit_spatial_models <- function(df, W, formula, config) {
  p_data <- pdata.frame(df, index = c("id_pump", "time_index"))
  fitted_models <- list()
  
  for (model_name in names(config$spatial_models)) {
    m_conf <- config$spatial_models[[model_name]]
    log_msg("    -> MODEL: Estimating ", model_name, "...")
    
    fit <- tryCatch({
      if (m_conf$pkg == "splm") {
        spgm(formula, data = p_data, listw = W, model = "within", method = "w2sls",
             lag = m_conf$lag, spatial.error = m_conf$spatial.error, Durbin = m_conf$Durbin)
      } else {
        plm(formula, data = p_data, model = "within", effect = "individual")
      }
    }, error = function(e) {
      log_msg("       [!] FAILED: ", model_name, " | Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(fit)) {
      fitted_models[[model_name]] <- fit
    }
  }
  
  return(fitted_models)
}


# ------------------------------------------------------------------------------
# UPDATED EXECUTION LOOP
# ------------------------------------------------------------------------------
run_rolling_estimation <- function(base_data_path, config, formula, radiuses, window_size, 
                                   estimation_granularity = "nuts2", output_dir,
                                   sample_fraction = 1.0, random_seed = 42) {
  
  set.seed(random_seed)
  setup_logging(output_dir, config)
  
  total_models_attempted <- 0
  total_models_failed <- 0
  
  log_msg("LOADING DATA")
  base_data <- readRDS(base_data_path)
  
  if (!is.numeric(base_data$df$time_index)) {
    log_msg("WARNING: time_index is not numeric. Forcing conversion to sequential integers.")
    base_data$df <- base_data$df %>%
      mutate(time_index = as.integer(factor(time_index, ordered = TRUE)))
  }
  
  # Apply sampling to reduce vector memory limits
  working_df <- apply_stratified_sampling(base_data$df, estimation_granularity, sample_fraction, random_seed)
  
  # --- NEW: SAVE RUN CONFIGURATION AND SAMPLED IDs ---
  run_config <- list(
    timestamp = Sys.time(),
    config_params = config,
    formula = deparse(formula),
    radiuses = radiuses,
    window_size = window_size,
    estimation_granularity = estimation_granularity,
    sample_fraction = sample_fraction,
    random_seed = random_seed,
    sampled_id_pumps = unique(working_df$id_pump)
  )
  
  config_file_name <- paste0("run_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RDS")
  config_path <- file.path(output_dir, config_file_name)
  saveRDS(run_config, config_path)
  log_msg("Run configuration and sampled id_pump list saved to: ", config_path)
  # ---------------------------------------------------
  
  log_msg("CLEARING MEMORY - remove base_data")
  rm(base_data)
  gc()
  
  regions <- unique(working_df[[estimation_granularity]])
  min_week <- min(working_df$time_index, na.rm = TRUE)
  max_week <- max(working_df$time_index, na.rm = TRUE)
  
  log_msg("Global time_index range: ", min_week, " to ", max_week)
  
  for (region_name in regions) {
    if(is.na(region_name)) next
    log_msg("--- PROCESSING REGION: ", region_name, " ---")
    
    region_df <- working_df %>% filter(!!sym(estimation_granularity) == region_name)
    
    for (rad in radiuses) {
      log_msg("=== RADIUS: ", rad, " meters ===")
      
      start_week <- min_week
      while ((start_week + window_size - 1) <= max_week) {
        end_week <- start_week + window_size - 1
        log_msg("WINDOW [", start_week, " - ", end_week, "]")
        
        window_data <- prepare_window_data(region_df, start_week, window_size)
        if (is.null(window_data)) {
          start_week <- start_week + 1
          next
        }
        
        aligned_data <- compute_distance_matrix(window_data, rad)
        if (is.null(aligned_data)) {
          start_week <- start_week + 1
          next
        }
        
        fitted_models <- fit_spatial_models(aligned_data$df, aligned_data$W, formula, config)
        
        expected_models <- length(config$spatial_models)
        successful_models <- length(fitted_models)
        failed_models <- expected_models - successful_models
        
        total_models_attempted <- total_models_attempted + expected_models
        total_models_failed <- total_models_failed + failed_models
        
        log_msg("    -> FITS: ", successful_models, "/", expected_models, " models succeeded.")
        
        if (successful_models > 0) {
          payload <- list(
            models = fitted_models,
            window = window_size,
            start_week = start_week,
            end_week = end_week,
            region = region_name,
            radius = rad,
            estimation_granularity = estimation_granularity
          )
          
          file_name <- sprintf("res_%s_rad%d_w%03d_w%03d.RDS", 
                               gsub("[^A-Za-z0-9]", "_", region_name), 
                               rad, start_week, end_week)
          
          log_msg(paste("SAVING MODEL DATA TO", file.path(output_dir, file_name)))
          create_coef_df(payload, file.path(output_dir, file_name))
        }
        
        rm(window_data, aligned_data, fitted_models, payload)
        gc(full = TRUE)
        
        start_week <- start_week + 1
      }
    }
  }
  
  fail_rate <- (total_models_failed / max(1, total_models_attempted)) * 100
  log_msg("=== PIPELINE COMPLETE ===")
  log_msg(sprintf("Total Models Attempted: %d", total_models_attempted))
  log_msg(sprintf("Total Models Failed:    %d", total_models_failed))
  log_msg(sprintf("Failure Rate:           %.2f%%", fail_rate))
}

# ------------------------------------------------------------------------------
# UPDATED RESULT COLLECTION
# ------------------------------------------------------------------------------
collect_results <- function(directory_path, output_path, pattern = "\\.RDS$") {
  
  file_paths <- list.files(path = directory_path, pattern = pattern, full.names = TRUE)
  
  # Prevent reading the configuration files as model output
  file_paths <- file_paths[!grepl("run_config", file_paths)]
  
  if (length(file_paths) == 0) {
    warning(paste("No result files matching pattern", pattern, "found in", directory_path))
    return(NULL)
  }
  
  combined_df <- map_dfr(file_paths, function(file) {
    tryCatch({
      readRDS(file)
    }, error = function(e) {
      warning(paste("Failed to read", file, ":", e$message))
      return(NULL)
    })
  })
  
  # saveRDS must execute before return()
  saveRDS(combined_df, output_path)
  return(combined_df)
}


# base_data <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/dataset.RDS")
base_data_path <- "/Volumes/T7 Shield/FRES/fuels_data/dataset.RDS"
output_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/test2"
formula <- mean_price_gasoline_self ~ brent_diff_pos + brent_diff_neg
radiuses <- c(2500, 5000)
window_size <- 52
random_seed <- 42

run_rolling_estimation(base_data_path, config, formula, radiuses, window_size, 
                       estimation_granularity = "nuts1", output_dir,
                       sample_fraction = 0.05, random_seed)

results <- collect_results(output_dir, output_path)




