# ------------------------------------------------------------------------------
# 3. ROLLING WINDOW EXECUTION (UPDATED WITH CONFIGURATION DICTIONARY)
# ------------------------------------------------------------------------------
library(plm)
library(splm)
library(spdep)
library(dplyr)

run_rolling_panel_models <- function(base_data, config, output_dir, window_size = 52, radius_m = 30000, 
                                     estimation_granularity = "nuts1", seed = 42) {
  
  set.seed(seed)
  df_all <- base_data$df
  stations_all_sf <- base_data$stations_sf
  
  parts_dir <- file.path(output_dir, "model_parts")
  if (!dir.exists(parts_dir)) dir.create(parts_dir, recursive = TRUE)
  
  subregions <- unique(stations_all_sf[[estimation_granularity]])
  subregions <- subregions[!is.na(subregions)]
  all_saved_files <- character()
  
  cat(sprintf("\n=== Execution Granularity: %s (Total Subregions: %d) ===\n", toupper(estimation_granularity), length(subregions)))
  
  for (sub_name in subregions) {
    cat(sprintf("\n--- Processing Subregion: %s ---\n", sub_name))
    
    stations_sub_sf <- stations_all_sf %>% filter(!!sym(estimation_granularity) == sub_name)
    df_sub <- df_all %>% filter(id_pump %in% stations_sub_sf$id_pump)
    
    unique_weeks <- sort(unique(df_sub$time_index))
    num_windows <- length(unique_weeks) - window_size + 1
    
    if (num_windows <= 0) {
      cat("  -> Skipped: Not enough weeks of data.\n")
      next
    }
    
    expected_files <- character(num_windows)
    window_labels <- character(num_windows)
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
    
    if (length(missing_idx) == 0) next
    
    for (i in missing_idx) {
      window_weeks <- unique_weeks[i:(i + window_size - 1)]
      win_label <- window_labels[i]
      part_filename <- expected_files[i]
      
      cat(sprintf("  Window %d/%d: %s\n", i, num_windows, win_label))
      
      df_window <- df_sub %>% filter(time_index %in% window_weeks)
      complete_pumps <- df_window %>%
        filter(complete.cases(mean_price_gasoline_self, latitude, longitude, population_density_km2, brand_group, station_type)) %>%
        count(id_pump) %>% filter(n == window_size) %>% pull(id_pump)
      
      if (length(complete_pumps) == 0) next
      
      df_window <- df_window %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump, time_index)
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
      
      if (nrow(stations_window_sf) < 2) next
      
      W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), style = "W", zero.policy = TRUE)
      df_panel <- pdata.frame(df_window, index = c("id_pump", "time_index"))
      
      # Determine active conditional features
      valid_conditionals <- Filter(function(feat) { length(unique(df_window[[feat]])) > 1 }, config$conditional_features)
      active_features <- c(config$base_features, valid_conditionals)
      
      window_models <- list()
      for (m_name in names(config$spatial_models)) {
        spec <- config$spatial_models[[m_name]]
        
        tryCatch({
          if (spec$pkg == "splm") {
            dynamic_formula <- as.formula(paste(config$dependent_var, "~", paste(active_features, collapse = " + ")))
            window_models[[m_name]] <- spgm(
              dynamic_formula, data = df_panel, listw = W, 
              lag = spec$lag, spatial.error = spec$spatial.error, Durbin = spec$Durbin, method = "w2sls"
            )
            
          } else if (spec$pkg == "plm") {
            # For PLM (e.g. SLX), we manually generate the WX columns with safe names
            slx_data <- df_window
            dynamic_formula <- as.formula(paste(config$dependent_var, "~", paste(active_features, collapse = " + ")))
            
            if (spec$Durbin) {
              X_mat <- model.matrix(dynamic_formula, data = df_panel)
              X_mat <- X_mat[, colnames(X_mat) != "(Intercept)", drop = FALSE]
              
              # Sanitize column names for formula compatibility (replace brackets, parens, colons)
              safe_col_names <- gsub("[^A-Za-z0-9_]", "_", colnames(X_mat))
              
              WX_mat <- matrix(0, nrow = nrow(X_mat), ncol = ncol(X_mat))
              colnames(WX_mat) <- paste0("Wx_", safe_col_names)
              
              time_ids <- as.character(df_panel$time_index)
              for (t_idx in unique(time_ids)) {
                idx <- which(time_ids == t_idx)
                for (c_idx in seq_len(ncol(X_mat))) {
                  WX_mat[idx, c_idx] <- lag.listw(W, X_mat[idx, c_idx], zero.policy = TRUE)
                }
              }
              
              slx_data <- cbind(slx_data, as.data.frame(WX_mat))
              slx_features <- c(active_features, colnames(WX_mat))
              dynamic_formula <- as.formula(paste(config$dependent_var, "~", paste(slx_features, collapse = " + ")))
            }
            
            p_data_slx <- pdata.frame(slx_data, index = c("id_pump", "time_index"))
            window_models[[m_name]] <- plm(dynamic_formula, data = p_data_slx, model = "pooling")
          }
        }, error = function(e) cat(sprintf("    -> %s failed: %s\n", m_name, e$message)))
      }
      
      saveRDS(list(models = window_models, end_week = window_weeks[window_size], window = win_label, region = sub_name), part_filename)
      all_saved_files <- c(all_saved_files, part_filename)
      
      rm(df_window, stations_window_sf, coords, nb, W, df_panel, window_models)
      gc()
    }
  }
  return(list(file_paths = all_saved_files, estimation_granularity = estimation_granularity))
}