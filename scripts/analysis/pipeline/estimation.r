library(foreach)
library(doParallel)
library(dplyr)
library(plm)
library(splm)
library(spdep)
library(sf)

run_rolling_panel_models <- function(base_data, config, output_dir, window_size, radius_m, estimation_granularity, seed = 42) {
  
  parts_dir <- file.path(output_dir, "model_parts")
  if (!dir.exists(parts_dir)) dir.create(parts_dir, recursive = TRUE)
  
  df <- base_data$df
  stations_sf <- base_data$stations_sf
  
  time_periods <- sort(unique(df$time_index))
  num_periods <- length(time_periods)
  
  if (num_periods < window_size) stop("Not enough time periods for the requested window.")
  
  start_indices <- 1:(num_periods - window_size + 1)
  regions <- unique(df[[estimation_granularity]])
  
  # Setup Parallel Backend
  n_cores <- parallel::detectCores() - 1
  cl <- parallel::makeCluster(n_cores, outfile = "")
  registerDoParallel(cl)
  
  cat(sprintf("\nStarting parallel estimation on %d cores...\n", n_cores))
  
  # Parallel loop over time windows
  rolling_output <- foreach(
    i = start_indices, 
    .packages = c("dplyr", "plm", "splm", "spdep", "sf"),
    .errorhandling = "pass"
  ) %dopar% {
    
    worker_pid <- Sys.getpid()
    start_week <- time_periods[i]
    end_week <- time_periods[i + window_size - 1]
    
    cat(sprintf("[Worker PID: %d] Processing Window: %s to %s\n", worker_pid, start_week, end_week))
    
    window_results <- list()
    df_window <- df %>% filter(time_index >= start_week & time_index <= end_week)
    
    # Iterate over regions within the window
    for (reg in regions) {
      
      df_reg <- df_window %>% filter(!!sym(estimation_granularity) == reg)
      
      # 1. Ensure strictly balanced panel
      complete_pumps <- df_reg %>%
        filter(complete.cases(mean_price_gasoline_self, latitude, longitude, population_density_km2, brand_group, station_type)) %>%
        count(id_pump) %>% filter(n == window_size) %>% pull(id_pump)
      
      if (length(complete_pumps) < 30) next
      
      df_sample <- df_reg %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump, time_index)
      st_sample <- stations_sf %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump)
      
      # 2. Compute Spatial Weights (W)
      coords <- st_coordinates(st_sample)
      nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
      
      isolated <- which(card(nb) == 0)
      if (length(isolated) > 0) {
        st_sample <- st_sample[-isolated, ]
        df_sample <- df_sample %>% filter(id_pump %in% st_sample$id_pump)
        coords <- st_coordinates(st_sample)
        nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
      }
      
      if (nrow(st_sample) < 30) next
      
      W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
                    style = "W", zero.policy = TRUE)
      
      p_data <- pdata.frame(df_sample, index = c("id_pump", "time_index"))
      
      # 3. Model Formula
      base_vars <- paste(config$base_features, collapse = " + ")
      cond_vars <- paste(config$conditional_features, collapse = " + ")
      form <- as.formula(paste(config$dependent_var, "~", base_vars, "+", cond_vars))
      
      models_fit <- list()
      
      # 4. Estimate Configured Models
      for (m_name in names(config$spatial_models)) {
        m_conf <- config$spatial_models[[m_name]]
        
        fit <- tryCatch({
          if (m_conf$pkg == "splm") {
            spgm(form, data = p_data, listw = W, lag = m_conf$lag, 
                 spatial.error = m_conf$spatial.error, Durbin = m_conf$Durbin, method = "w2sls")
          } else {
            # Base pooling model for non-spatial baselines
            plm(form, data = p_data, model = "pooling")
          }
        }, error = function(e) NULL)
        
        if (!is.null(fit)) models_fit[[m_name]] <- fit
      }
      
      # 5. Save Window-Region Parts
      if (length(models_fit) > 0) {
        safe_reg <- gsub("[^A-Za-z0-9]", "_", reg)
        part_name <- sprintf("part_w%03d_%s.RDS", i, safe_reg)
        
        saveRDS(list(
          region = reg,
          window = i,
          start_week = start_week,
          end_week = end_week,
          models = models_fit
        ), file.path(parts_dir, part_name))
        
        window_results[[reg]] <- part_name
      }
    }
    
    return(window_results)
  }
  
  stopCluster(cl)
  cat("\nParallel estimation complete.\n")
  
  return(rolling_output)
}