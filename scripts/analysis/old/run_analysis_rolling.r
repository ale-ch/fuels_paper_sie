# ------------------------------------------------------------------------------
# 1. BASE DATA PREPARATION (Run Once to avoid I/O bottlenecks)
# ------------------------------------------------------------------------------
prepare_base_data <- function(data_file, stations_list_file) {
  require(tidyverse)
  require(sf)
  require(quantmod)
  require(zoo)
  require(lubridate)
  
  # Load Panel Data
  df <- readRDS(data_file)
  
  # Load and Clean Stations
  stations_list <- read_delim(stations_list_file, delim = "|", show_col_types = FALSE) %>%
    select(-c(address, contains("Estrazione"))) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude")) %>%
    st_set_crs(4326) %>%
    st_transform(32632) %>%
    arrange(id_pump)
  
  # Fetch Brent Crude Data
  getSymbols("DCOILBRENTEU", src = "FRED", from = "2016-01-01", to = "2026-12-31", auto.assign = TRUE)
  brent_df <- data.frame(
    date = ymd(index(DCOILBRENTEU)),
    brent_price = as.numeric(DCOILBRENTEU$DCOILBRENTEU)
  ) %>%
    complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    mutate(brent_price = na.locf(brent_price, na.rm = FALSE))
  
  brent_weekly_df <- brent_df %>% 
    group_by(year = year(date), week = week(date)) %>% 
    reframe(brent_price_mean = mean(brent_price, na.rm = TRUE))
  
  # Merge Brent and Create Time Index
  df <- left_join(df, brent_weekly_df, by = c("year", "week")) %>%
    mutate(time_index = paste(year, sprintf("%02d", week), sep = "-"))
  
  return(list(df = df, stations_sf = stations_list))
}

# ------------------------------------------------------------------------------
# 2. FUNCTION: RUN ROLLING WINDOW MODELS
# ------------------------------------------------------------------------------
analyze_rolling_diagnostics <- function(base_data, window_size = 52, radius_m = 30000) {
  require(spdep)
  require(plm)
  require(splm)
  require(dplyr)
  require(sf)
  
  df_all <- base_data$df
  stations_all_sf <- base_data$stations_sf
  
  unique_weeks <- sort(unique(df_all$time_index))
  num_windows <- length(unique_weeks) - window_size + 1
  
  models_list <- list()
  
  for (i in seq_len(num_windows)) {
    window_weeks <- unique_weeks[i:(i + window_size - 1)]
    start_week <- window_weeks[1]
    end_week <- window_weeks[window_size]
    window_label <- paste(start_week, end_week, sep = "_to_")
    
    cat(sprintf("Processing Window %d/%d: %s\n", i, num_windows, window_label))
    
    tryCatch({
      # Subset data to current window
      df_window <- df_all %>% filter(time_index %in% window_weeks)
      
      # Balance Panel: Keep stations with full observations in this window
      complete_pumps <- df_window %>%
        filter(!is.na(mean_price_gasoline_self), !is.na(latitude), !is.na(longitude)) %>%
        group_by(id_pump) %>%
        tally() %>%
        filter(n == window_size) %>%
        pull(id_pump)
      
      df_window <- df_window %>% 
        filter(id_pump %in% complete_pumps) %>%
        arrange(id_pump)
      
      # Subset and sort spatial data
      stations_window_sf <- stations_all_sf %>%
        filter(id_pump %in% complete_pumps) %>%
        arrange(id_pump)
      
      # Rebuild Spatial Weights Matrix (W)
      coords <- st_coordinates(stations_window_sf)
      nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
      
      # Identify and remove isolated stations (0 neighbors)
      isolated_idx <- which(card(nb) == 0)
      if (length(isolated_idx) > 0) {
        isolated_pumps <- stations_window_sf$id_pump[isolated_idx]
        stations_window_sf <- stations_window_sf %>% filter(!(id_pump %in% isolated_pumps))
        df_window <- df_window %>% filter(!(id_pump %in% isolated_pumps))
        
        coords <- st_coordinates(stations_window_sf)
        nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
      }
      
      dist_list <- nbdists(nb, coords)
      inv_dist <- lapply(dist_list, function(x) ifelse(x > 0, 1 / x, 0))
      W <- nb2listw(nb, glist = inv_dist, style = "W", zero.policy = TRUE)
      
      # Convert to plm panel object
      df_panel <- pdata.frame(df_window, index = c("id_pump", "time_index"))
      
      # Run Model
      model <- spgm(
        mean_price_gasoline_self ~ lag(mean_price_gasoline_self, 1) + lag(brent_price_mean, 1),
        data = df_panel, 
        listw = W, 
        lag = TRUE, 
        Durbin = FALSE, 
        spatial.error = FALSE, 
        method = "w2sls"
      )
      
      models_list[[window_label]] <- list(
        model = model,
        start_week = start_week,
        end_week = end_week,
        n_stations = nrow(stations_window_sf)
      )
      
    }, error = function(e) {
      message(sprintf("Failed for window %s: %s", window_label, e$message))
    })
  }
  
  return(models_list)
}


# ------------------------------------------------------------------------------
# 3. FUNCTION: ANALYZE ROLLING RESULTS (FULL DIAGNOSTIC PANEL)
# ------------------------------------------------------------------------------
analyze_rolling_diagnostics <- function(models_list) {
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  # 1. Extract all relevant statistics
  coef_df <- do.call(rbind, lapply(names(models_list), function(win_label) {
    mod_data <- models_list[[win_label]]
    summ <- summary(mod_data$model)
    
    coefs <- as.data.frame(summ$CoefTable)
    colnames(coefs) <- c("estimate", "std_error", "t_value", "p_value")
    coefs$term <- rownames(coefs)
    coefs$window <- win_label
    coefs$end_week <- mod_data$end_week
    coefs$n_stations <- mod_data$n_stations # Track active stations per window
    return(coefs)
  }))
  
  rownames(coef_df) <- NULL
  
  # 2. Clean data and compute dates
  coef_df <- coef_df %>%
    mutate(
      date = as.Date(paste0(end_week, "-1"), format = "%Y-%W-%u"),
      term = case_when(
        term == "lambda" ~ "Spatial Lag (rho)",
        term == "lag(mean_price_gasoline_self, 1)" ~ "Time Lag (Gasoline t-1)",
        term == "lag(brent_price_mean, 1)" ~ "Time Lag (Brent t-1)",
        TRUE ~ term
      )
    ) %>%
    filter(term != "(Intercept)")
  
  # 3. Plot A: Coefficient Estimates with 95% Confidence Intervals
  p_coef <- ggplot(coef_df, aes(x = date, y = estimate, color = term, group = term)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = estimate - 1.96 * std_error, 
                    ymax = estimate + 1.96 * std_error, fill = term), 
                alpha = 0.2, color = NA) +
    facet_wrap(~ term, scales = "free_y", ncol = 1) +
    scale_x_date(date_labels = "%Y/%m", date_breaks = "6 months") +
    theme_minimal() +
    labs(
      title = "A. Coefficient Estimates (95% CI)",
      x = "Window End Date (Year/Month)",
      y = "Estimate"
    ) +
    theme(legend.position = "none", strip.text = element_text(face = "bold", size = 11))
  
  # 4. Plot B: P-Values (Log10 Scale)
  p_pval <- ggplot(coef_df, aes(x = date, y = p_value, color = term, group = term)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0.01, linetype = "dotted", color = "blue") +
    geom_line(linewidth = 1) +
    geom_point(size = 1) +
    facet_wrap(~ term, scales = "free_y", ncol = 1) +
    scale_y_log10(labels = scales::scientific) +
    scale_x_date(date_labels = "%Y/%m", date_breaks = "6 months") +
    theme_minimal() +
    labs(
      title = "B. P-Values (Log10 Scale)",
      subtitle = "Red Dashed = 0.05, Blue Dotted = 0.01",
      x = "Window End Date (Year/Month)",
      y = "P-Value"
    ) +
    theme(legend.position = "none", strip.text = element_text(face = "bold", size = 11))
  
  # 5. Plot C: Absolute T-Values
  p_tval <- ggplot(coef_df, aes(x = date, y = abs(t_value), color = term, group = term)) +
    geom_hline(yintercept = 1.96, linetype = "dashed", color = "red") +
    geom_line(linewidth = 1) +
    facet_wrap(~ term, scales = "free_y", ncol = 1) +
    scale_x_date(date_labels = "%Y/%m", date_breaks = "6 months") +
    theme_minimal() +
    labs(
      title = "C. Absolute T-Values",
      subtitle = "Red Dashed = 1.96 (Approx. 95% Significance Threshold)",
      x = "Window End Date (Year/Month)",
      y = "|T-Value|"
    ) +
    theme(legend.position = "none", strip.text = element_text(face = "bold", size = 11))
  
  # Print plots to graphical device
  print(p_coef)
  print(p_pval)
  print(p_tval)
  
  # Return data and plot objects for external saving/manipulation
  return(list(
    stats_data = coef_df,
    plots = list(
      coefficients = p_coef,
      p_values = p_pval,
      t_values = p_tval
    )
  ))
}




# ------------------------------------------------------------------------------
# 4. EXECUTION SCRIPT
# ------------------------------------------------------------------------------
file_path <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/part_0.RDS"
stations_list_file <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"

# 1. Load data once
base_data <- prepare_base_data(file_path, stations_list_file)

# 2. Run rolling models (e.g., 52 weeks = 1 year rolling window)
rolling_models <- run_rolling_models(base_data, window_size = 52)

# 3. Analyze and plot results
rolling_analysis <- analyze_rolling_diagnostics(rolling_models)