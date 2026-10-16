# ------------------------------------------------------------------------------
# estimation.r (UPDATED FOR NA PLACEHOLDERS & INTERPOLATION)
# ------------------------------------------------------------------------------
library(foreach)
library(doParallel)
library(dplyr)
library(tidyverse)
library(sf)
library(spdep)
library(plm)
library(splm)

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

create_coef_df <- function(mod_data, output_path) {
  
  # Loop over ALL expected models, not just the ones that succeeded
  c_df_part <- map_dfr(names(config$spatial_models), function(m_name) {
    
    # Baseline placeholder for failures
    base_df <- data.frame(
      estimate = NA_real_, std_error = NA_real_, t_value = NA_real_, p_value = NA_real_,
      term = NA_character_, model = m_name,
      window = mod_data$window, radius_m = mod_data$radius,
      end_week = mod_data$end_week, region = mod_data$region,
      estimation_granularity = mod_data$estimation_granularity,
      date = as.Date("2016-01-01") + (mod_data$start_week - 1) * 7,
      end_date = as.Date("2016-01-01") + (mod_data$end_week - 1) * 7,
      stringsAsFactors = FALSE
    )
    
    # If model failed entirely during this window, return the NA placeholder
    if (!(m_name %in% names(mod_data$models))) return(base_df)
    
    summ <- tryCatch({
      summary(mod_data$models[[m_name]])
    }, error = function(e) {
      return(NULL) 
    })
    
    if (is.null(summ)) return(base_df)
    
    if ("CoefTable" %in% names(summ)) {
      c_df <- as.data.frame(summ$CoefTable)
    } else if ("coefficients" %in% names(summ)) {
      c_df <- as.data.frame(summ$coefficients)
    } else {
      return(base_df)
    }
    
    if (nrow(c_df) == 0 || ncol(c_df) < 4) {
      return(base_df)
    }
    
    c_df <- c_df[, 1:4]
    colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
    
    c_df$term <- rownames(c_df)
    c_df$model <- m_name
    c_df$window <- mod_data$window
    c_df$radius_m <- mod_data$radius
    c_df$end_week <- mod_data$end_week
    c_df$region <- mod_data$region
    c_df$estimation_granularity <- mod_data$estimation_granularity
    c_df$date <- base_df$date
    c_df$end_date <- base_df$end_date
    
    return(c_df)
  })
  
  rownames(c_df_part) <- NULL
  
  c_df_part$term <- sapply(c_df_part$term, function(x) {
    if (is.na(x)) return(NA_character_)
    
    if (x %in% names(config$term_labels)) {
      return(config$term_labels[[x]])
    } else if (grepl("W\\*", x)) { 
      base_var <- gsub("W\\*", "", x)
      clean_base <- ifelse(base_var %in% names(config$term_labels), config$term_labels[[base_var]], base_var)
      return(paste("Wx", clean_base))
    } else if (grepl("^Wx_", x)) {
      base_var <- gsub("^Wx_", "", x)
      clean_base <- ifelse(base_var %in% names(config$term_labels), config$term_labels[[base_var]], base_var)
      return(paste("Wx", clean_base))
    } else {
      return(x)
    }
  })
  
  saveRDS(c_df_part, output_path)
  return(c_df_part)
}

apply_stratified_sampling <- function(df, granularity, sample_fraction, random_seed = 42) {
  if(granularity == "nuts0") granularity <- "nuts1"
  
  set.seed(random_seed)
  if (sample_fraction >= 1.0) return(df)
  
  log_msg("Applying stratified sampling (", sample_fraction * 100, "%) at ", granularity, " level.")
  
  sampled_pumps <- df %>%
    group_by(id_pump) %>% slice(1) %>% ungroup() %>%
    group_by(!!sym(granularity)) %>% slice_sample(prop = sample_fraction) %>%
    pull(id_pump)
  
  result_df <- df %>% filter(id_pump %in% sampled_pumps)
  log_msg("Retained ", length(unique(result_df$id_pump)), " unique stations for estimation.")
  
  return(result_df)
}

prepare_window_data <- function(df, start_week, window_size) {
  end_week <- start_week + window_size - 1
  window_df <- df %>% filter(time_index >= start_week, time_index <= end_week)
  
  if (nrow(window_df) == 0) return(NULL)
  
  complete_pumps <- window_df %>%
    filter(complete.cases(mean_price_gasoline_self, latitude, longitude), !is.nan(mean_price_gasoline_self)) %>%
    count(id_pump) %>% filter(n == window_size) %>% pull(id_pump)
  
  if (length(complete_pumps) < 10) return(NULL)
  
  window_df <- window_df %>% filter(id_pump %in% complete_pumps) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32632)
  window_df_unique <- window_df %>% group_by(id_pump) %>% slice(1) %>% ungroup()
  
  return(list(df = window_df, df_unique = window_df_unique))
}

compute_distance_matrix <- function(window_data, radius_m) {
  df <- window_data$df
  df_unique <- window_data$df_unique
  
  coords <- st_coordinates(df_unique)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
  
  isolated <- which(card(nb) == 0)
  if (length(isolated) > 0) df_unique <- df_unique[-isolated, ]
  if (nrow(df_unique) < 10) return(NULL)
  
  df <- df %>% filter(id_pump %in% df_unique$id_pump) %>% arrange(id_pump, time_index)
  df_unique <- df_unique %>% arrange(id_pump)
  
  coords <- st_coordinates(df_unique)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
  
  W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
                style = "W", zero.policy = TRUE)
  return(list(df = df, W = W))
}

fit_spatial_models <- function(df, W, formula, config) {
  p_data <- pdata.frame(df, index = c("id_pump", "time_index"))
  fitted_models <- list()
  
  for (model_name in names(config$spatial_models)) {
    m_conf <- config$spatial_models[[model_name]]
    fit <- tryCatch({
      if (m_conf$pkg == "splm") {
        spgm(formula, data = p_data, listw = W, model = "within", method = "w2sls",
             lag = m_conf$lag, spatial.error = m_conf$spatial.error, Durbin = m_conf$Durbin)
      } else {
        plm(formula, data = p_data, model = "within", effect = "individual")
      }
    }, error = function(e) { return(NULL) })
    
    if (!is.null(fit)) fitted_models[[model_name]] <- fit
  }
  return(fitted_models)
}

# ------------------------------------------------------------------------------
# UPDATED EXECUTION LOOP WITH PLACEHOLDER INJECTION
# ------------------------------------------------------------------------------
run_rolling_estimation <- function(base_data_path, config, formula, radiuses, window_size, step_size = 1,
                                   estimation_granularity = c("nuts0", "nuts1", "nuts2", "nuts3"), output_dir,
                                   sample_fraction = 1.0, random_seed = 42) {
  
  match.arg(estimation_granularity)
  set.seed(random_seed)
  
  total_models_attempted <- 0
  total_models_failed <- 0
  
  log_msg("LOADING DATA")
  base_data <- readRDS(base_data_path)
  
  if (!is.numeric(base_data$df$time_index)) {
    base_data$df <- base_data$df %>% mutate(time_index = as.integer(factor(time_index, ordered = TRUE)))
  }
  
  working_df <- apply_stratified_sampling(base_data$df, estimation_granularity, sample_fraction, random_seed)
  
  run_config <- list(
    timestamp = Sys.time(), config_params = config, formula = deparse(formula),
    radiuses = radiuses, window_size = window_size, step_size = step_size,
    estimation_granularity = estimation_granularity, sample_fraction = sample_fraction,
    random_seed = random_seed, sampled_id_pumps = unique(working_df$id_pump)
  )
  
  config_path <- file.path(output_dir, paste0("run_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RDS"))
  saveRDS(run_config, config_path)
  
  rm(base_data)
  gc()
  
  if(estimation_granularity == "nuts0") {
    regions <- "Italy"
  } else {
    regions <- unique(working_df[[estimation_granularity]])
  }
  
  min_week <- min(working_df$time_index, na.rm = TRUE)
  max_week <- max(working_df$time_index, na.rm = TRUE)
  valid_regions <- regions[!is.na(regions)]
  start_weeks_seq <- seq(min_week, max_week - window_size + 1, by = step_size)
  
  total_windows <- length(start_weeks_seq)
  total_iterations <- length(valid_regions) * length(radiuses) * total_windows
  current_iteration <- 0
  computed_iterations <- 0
  loop_start_time <- as.numeric(Sys.time())
  
  for (region_name in valid_regions) {
    region_df <- if(region_name == "Italy") working_df else working_df %>% filter(!!sym(estimation_granularity) == region_name)
    
    for (rad in radiuses) {
      for (start_week in start_weeks_seq) {
        end_week <- start_week + window_size - 1
        current_iteration <- current_iteration + 1
        
        file_name <- sprintf("res_%s_rad%d_w%03d_w%03d.RDS", gsub("[^A-Za-z0-9]", "_", region_name), rad, start_week, end_week)
        target_file_path <- file.path(output_dir, file_name)
        
        pct <- (current_iteration / total_iterations) * 100
        if (computed_iterations > 0) {
          elapsed <- as.numeric(Sys.time()) - loop_start_time
          eta_secs <- (total_iterations - current_iteration) * (elapsed / computed_iterations)
          eta_h <- floor(eta_secs / 3600); eta_m <- floor((eta_secs %% 3600) / 60); eta_s <- floor(eta_secs %% 60)
        } else { eta_h <- 0; eta_m <- 0; eta_s <- 0 }
        
        cat(sprintf("\r[%5.1f%%] Iter: %d/%d | Reg: %-6s | Rad: %-5d | Win: %03d-%03d | ETA: %02d:%02d:%02d          ", 
                    pct, current_iteration, total_iterations, substr(region_name, 1, 6), rad, start_week, end_week, eta_h, eta_m, eta_s))
        
        if (file.exists(target_file_path)) next
        computed_iterations <- computed_iterations + 1
        
        tryCatch({
          window_data <- prepare_window_data(region_df, start_week, window_size)
          if (is.null(window_data)) stop("Insufficient data points")
          
          aligned_data <- compute_distance_matrix(window_data, rad)
          if (is.null(aligned_data)) stop("Spatial matrix failed")
          
          fitted_models <- fit_spatial_models(aligned_data$df, aligned_data$W, formula, config)
          
          expected_models <- length(config$spatial_models)
          successful_models <- length(fitted_models)
          
          total_models_attempted <<- total_models_attempted + expected_models
          total_models_failed <<- total_models_failed + (expected_models - successful_models)
          
          payload <- list(
            models = fitted_models, window = window_size, start_week = start_week,
            end_week = end_week, region = region_name, radius = rad, estimation_granularity = estimation_granularity
          )
          create_coef_df(payload, target_file_path)
          
        }, error = function(e) {
          # Log failure to console quietly, update failure counts, and write NA placeholders
          cat(sprintf("\n    [!] FAILED: Win %03d-%03d (%s) -> %s. Saving NAs.\n", start_week, end_week, region_name, e$message))
          
          expected_models <- length(config$spatial_models)
          total_models_attempted <<- total_models_attempted + expected_models
          total_models_failed <<- total_models_failed + expected_models
          
          payload <- list(
            models = list(), # Completely empty, forcing create_coef_df to generate NAs
            window = window_size, start_week = start_week, end_week = end_week,
            region = region_name, radius = rad, estimation_granularity = estimation_granularity
          )
          create_coef_df(payload, target_file_path)
        })
        
        # Clean environment before next iteration
        env_vars <- ls()
        to_remove <- intersect(env_vars, c("window_data", "aligned_data", "fitted_models", "payload"))
        if(length(to_remove) > 0) suppressWarnings(rm(list = to_remove))
        gc(full = TRUE)
      }
    }
  }
  
  cat("\n") 
  fail_rate <- (total_models_failed / max(1, total_models_attempted)) * 100
  log_msg("=== PIPELINE COMPLETE ===")
  log_msg(sprintf("Total Models Attempted: %d", total_models_attempted))
  log_msg(sprintf("Total Models Failed:    %d", total_models_failed))
  log_msg(sprintf("Failure Rate:           %.2f%%", fail_rate))
}

collect_results <- function(directory_path, output_path, pattern = "\\.RDS$") {
  file_paths <- list.files(path = directory_path, pattern = pattern, full.names = TRUE)
  file_paths <- file_paths[!grepl("run_config", file_paths)]
  
  if (length(file_paths) == 0) return(NULL)
  
  combined_df <- map_dfr(file_paths, function(file) {
    tryCatch({ readRDS(file) }, error = function(e) { return(NULL) })
  })
  
  saveRDS(combined_df, output_path)
  return(combined_df)
}