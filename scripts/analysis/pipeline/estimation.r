# ------------------------------------------------------------------------------
# 2. MODEL SPECIFICATIONS
# ------------------------------------------------------------------------------
base_formula <- mean_price_gasoline_self ~ lag(mean_price_gasoline_self, 1) + 
  # lag(brent_price_mean, 1) + 
  lag(brent_diff_pos, 1) + 
  lag(brent_diff_neg, 1) + 
  population_density_km2 + brand_group + station_type

models_spec <- list(
  SLX   = list(lag = FALSE, spatial.error = FALSE, Durbin = TRUE),
  SEM   = list(lag = FALSE, spatial.error = TRUE,  Durbin = FALSE),
  SDEM  = list(lag = FALSE, spatial.error = TRUE,  Durbin = TRUE),
  SAR   = list(lag = TRUE,  spatial.error = FALSE, Durbin = FALSE),
  SDM   = list(lag = TRUE,  spatial.error = FALSE, Durbin = TRUE),
  SARAR = list(lag = TRUE,  spatial.error = TRUE,  Durbin = FALSE),
  GNS   = list(lag = TRUE,  spatial.error = TRUE,  Durbin = TRUE)
)

# ------------------------------------------------------------------------------
# 3. ROLLING WINDOW EXECUTION (UPDATED WITH CHECKPOINT SYSTEM & SUBREGIONS)
# ------------------------------------------------------------------------------
run_rolling_panel_models <- function(base_data, output_dir, window_size = 52, radius_m = 30000, 
                                     estimation_granularity = "nuts1", seed = 42) {
  
  set.seed(seed)
  
  df_all <- base_data$df
  stations_all_sf <- base_data$stations_sf
  
  parts_dir <- file.path(output_dir, "model_parts")
  if (!dir.exists(parts_dir)) dir.create(parts_dir, recursive = TRUE)
  
  # Extract all unique subregions based on the chosen granularity
  subregions <- unique(stations_all_sf[[estimation_granularity]])
  subregions <- subregions[!is.na(subregions)]
  
  all_saved_files <- character()
  
  cat(sprintf("\n=== Execution Granularity: %s (Total Subregions: %d) ===\n", toupper(estimation_granularity), length(subregions)))
  
  for (sub_name in subregions) {
    cat(sprintf("\n--- Processing Subregion: %s ---\n", sub_name))
    
    # Subset data for the current subregion
    stations_sub_sf <- stations_all_sf %>% filter(!!sym(estimation_granularity) == sub_name)
    df_sub <- df_all %>% filter(id_pump %in% stations_sub_sf$id_pump)
    
    unique_weeks <- sort(unique(df_sub$time_index))
    num_windows <- length(unique_weeks) - window_size + 1
    
    if (num_windows <= 0) {
      cat("  -> Skipped: Not enough weeks of data for the requested window size.\n")
      next
    }
    
    # --- CHECKPOINT SYSTEM PRE-COMPUTATION ---
    expected_files <- character(num_windows)
    window_labels <- character(num_windows)
    
    # Replace spaces or special chars in sub_name to be safe for filenames
    safe_sub_name <- gsub("[^A-Za-z0-9]", "_", sub_name)
    
    for (i in seq_len(num_windows)) {
      window_weeks <- unique_weeks[i:(i + window_size - 1)]
      win_label <- paste(window_weeks[1], window_weeks[window_size], sep = "_to_")
      window_labels[i] <- win_label
      expected_files[i] <- file.path(parts_dir, sprintf("res_%s_%s.RDS", safe_sub_name, win_label))
    }
    
    existing_idx <- file.exists(expected_files)
    saved_files <- expected_files[existing_idx]
    missing_idx <- which(!existing_idx)
    
    all_saved_files <- c(all_saved_files, saved_files)
    
    if (length(missing_idx) == 0) {
      cat("  -> All expected model files found for this subregion. Skipping estimation.\n")
      next
    }
    
    if (length(saved_files) > 0) {
      cat(sprintf("  -> Checkpoint active: Found %d existing files. Resuming remaining %d windows.\n", 
                  length(saved_files), length(missing_idx)))
    }
    # -----------------------------------------
    
    for (i in missing_idx) {
      window_weeks <- unique_weeks[i:(i + window_size - 1)]
      win_label <- window_labels[i]
      part_filename <- expected_files[i]
      
      cat(sprintf("  Window %d/%d: %s\n", i, num_windows, win_label))
      
      df_window <- df_sub %>% filter(time_index %in% window_weeks)
      
      complete_pumps <- df_window %>%
        filter(complete.cases(mean_price_gasoline_self, latitude, longitude, 
                              population_density_km2, brand_group, station_type)) %>%
        count(id_pump) %>% filter(n == window_size) %>% pull(id_pump)
      
      if (length(complete_pumps) == 0) {
        cat("    -> Skipped: No complete pumps in this window.\n")
        next
      }
      
      df_window <- df_window %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump)
      stations_window_sf <- stations_sub_sf %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump)
      
      coords <- st_coordinates(stations_window_sf)
      nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
      isolated <- which(card(nb) == 0)
      
      if (length(isolated) > 0) {
        stations_window_sf <- stations_window_sf[-isolated, ]
        df_window <- df_window %>% filter(id_pump %in% stations_window_sf$id_pump)
        coords <- st_coordinates(stations_window_sf)
        nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
      }
      
      if (nrow(stations_window_sf) < 2) {
        cat("    -> Skipped: Insufficient connected stations.\n")
        next
      }
      
      W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
                    style = "W", zero.policy = TRUE)
      df_panel <- pdata.frame(df_window, index = c("id_pump", "time_index"))
      
      n_brand <- length(unique(as.character(df_window$brand_group)))
      n_station <- length(unique(as.character(df_window$station_type)))
      features <- c("lag(mean_price_gasoline_self, 1)", "lag(brent_diff_pos, 1)", "lag(brent_diff_neg, 1)", "population_density_km2")
      
      if (n_brand > 1) features <- c(features, "brand_group")
      if (n_station > 1) features <- c(features, "station_type")
      
      dynamic_formula <- as.formula(paste("mean_price_gasoline_self ~", paste(features, collapse = " + ")))
      
      window_models <- list()
      for (m_name in names(models_spec)) {
        spec <- models_spec[[m_name]]
        tryCatch({
          window_models[[m_name]] <- spgm(
            dynamic_formula, data = df_panel, listw = W, 
            lag = spec$lag, spatial.error = spec$spatial.error, Durbin = spec$Durbin, method = "w2sls"
          )
        }, error = function(e) cat(sprintf("    -> %s failed: %s\n", m_name, e$message)))
      }
      
      # The saved region dynamically adapts to the current active subregion based on granularity
      saveRDS(list(models = window_models, end_week = window_weeks[window_size], window = win_label, region = sub_name), part_filename)
      all_saved_files <- c(all_saved_files, part_filename)
      
      rm(df_window, stations_window_sf, coords, nb, W, df_panel, window_models)
      gc()
    }
  }
  
  return(list(
    file_paths = all_saved_files, 
    estimation_granularity = estimation_granularity
  ))
}