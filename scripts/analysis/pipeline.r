# ------------------------------------------------------------------------------
# 1. DATA PREPARATION
# ------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(quantmod)
library(zoo)
library(lubridate)
library(readxl)
library(spdep)
library(plm)
library(splm)
library(jsonlite)

build_population_data <- function(base_dir) {
  
  if(!file.exists(file.path(base_dir, "city_population.csv"))) {
    singola_area_dir <- file.path(base_dir, "PopolazioneEta-SingolaArea-Comuni")
    
    posas_files <- list.files(base_dir, pattern = "POSAS", full.names = TRUE)
    population_posas <- map_dfr(posas_files, function(file_path) {
      extracted_year <- as.numeric(str_extract(basename(file_path), "\\d{4}"))
      read_delim(file_path, delim = ";", skip = 1, show_col_types = FALSE) %>% 
        rename(id_city = `Codice comune`, city = Comune, population = Totale, age = Età) %>% 
        filter(age == 999, extracted_year > 2019) %>% 
        mutate(year = extracted_year) %>% 
        select(id_city, city, year, population)
    })
    
    process_singola_area <- function(file_path) {
      lines <- readLines(file_path, warn = FALSE)
      lines <- lines[lines != ""]
      meta_match <- str_match(str_remove_all(lines[1], '"'), "Comune:\\s*(\\d+)\\s*-\\s*(.+)$")
      
      if (is.na(meta_match[1])) return(NULL)
      
      header_idx <- grep("Età/Anno", lines)[1]
      totale_idx <- grep("^\"?Totale;", lines)[1]
      if (is.na(header_idx) || is.na(totale_idx)) return(NULL)
      
      read.csv(text = paste(lines[header_idx], lines[totale_idx], sep = "\n"), 
               sep = ";", check.names = FALSE, stringsAsFactors = FALSE) %>%
        pivot_longer(cols = -1, names_to = "year", values_to = "population") %>%
        mutate(id_city = as.numeric(meta_match[2]), city = meta_match[3], 
               year = as.numeric(year), population = as.numeric(population)) %>%
        select(id_city, city, year, population)
    }
    
    singola_area_files <- list.files(singola_area_dir, pattern = "\\.csv$", full.names = TRUE)
    
    pop_final <- bind_rows(
      mutate(population_posas, id_city = as.numeric(id_city)), 
      map_dfr(singola_area_files, process_singola_area)
    ) %>% filter(year >= 2016) %>% arrange(id_city, year)
    
    pop_final$city <- str_replace_all(toupper(pop_final$city), 
                                      c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'"))
    
    write_csv(pop_final, file.path(base_dir, "city_population.csv"))
  } else {
    pop_final <- read_csv(file.path(base_dir, "city_population.csv")) 
  }
  
  return(pop_final)
}

build_city_areas <- function(shp_path, nuts_path) {
  nuts_data <- fromJSON(nuts_path)
  nuts_data <- as.data.frame(nuts_data$resultset)
  nuts_data <- nuts_data %>% 
    select(PRO_COM, contains("DEN")) %>% 
    rename(
      nuts1 = DEN_RIP,
      nuts2 = DEN_REG,
      nuts3 = DEN_UTS
    )
  
  shp_data <- st_read(shp_path, quiet = TRUE)
  shp_data <- shp_data %>% 
    left_join(nuts_data)
  shp_data$COMUNE <- str_replace_all(toupper(shp_data$COMUNE), 
                                     c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'"))
  
  shp_data %>%
    mutate(COMUNE = ifelse(!is.na(COMUNE_A) & COMUNE_A != "", sub("/.*", "", COMUNE), COMUNE),
           area_sq_km = Shape_Area / 1000000) %>%
    rename(city = COMUNE) %>% 
    select(city, area_sq_km, contains("nuts")) %>% 
    st_drop_geometry()
}

build_brand_categories <- function(mappings_dir) {
  files_list <- list.files(mappings_dir, pattern = "RDS$", full.names = TRUE)
  brand_map <- map_dfr(files_list, function(f) select(readRDS(f), brand)) %>%
    count(brand, name = "n") %>% arrange(desc(n)) %>%
    mutate(brand_group = if_else(cumsum(n / sum(n)) <= 0.9, "major", "minor")) %>%
    select(brand, brand_group)
  
  brand_map
}

prepare_integrated_panel <- function(data_file, stations_list_file, pop_dir, shp_path, mappings_dir, test=FALSE) {
  df <- readRDS(data_file)
  
  stations_sf <- read_delim(stations_list_file, delim = "|", show_col_types = FALSE) %>%
    select(-c(address, contains("Estrazione"))) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(32632) %>% arrange(id_pump)
  
  # 1. Fetch and clean daily Brent data
  getSymbols("DCOILBRENTEU", src = "FRED", from = "2016-01-01", to = "2026-12-31", auto.assign = TRUE)
  brent_df <- data.frame(
    date = ymd(index(DCOILBRENTEU)), 
    brent_price = as.numeric(DCOILBRENTEU$DCOILBRENTEU)
  ) %>%
    complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    mutate(brent_price = na.locf(brent_price, na.rm = FALSE))
  
  # 2. Aggregate to weekly, THEN calculate differences
  brent_weekly_df <- brent_df %>%
    group_by(year = year(date), week = week(date)) %>%
    reframe(brent_price_mean = mean(brent_price, na.rm = TRUE)) %>%
    arrange(year, week) %>%
    mutate(
      brent_diff = brent_price_mean - dplyr::lag(brent_price_mean),
      brent_diff_pos = replace_na(ifelse(brent_diff > 0, brent_diff, 0), 0),
      brent_diff_neg = replace_na(ifelse(brent_diff < 0, brent_diff, 0), 0)
    )
  
  # 3. Join the complete weekly data to the main panel
  df <- df %>%
    left_join(brent_weekly_df, by = c("year", "week")) %>%
    mutate(time_index = paste(year, sprintf("%02d", week), sep = "-"),
           city = str_replace_all(toupper(city), c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'")))
  
  df <- df %>%
    left_join(
      build_population_data(pop_dir) %>% 
        select(city, year, population), by = c("city", "year")
    ) %>%
    left_join(build_city_areas(shp_path, nuts_path), by = "city") %>%
    mutate(population_density_km2 = population / area_sq_km)
  
  
  # Categorize brands
  if (file.exists("data/brand_categories.csv")) {
    brand_map <- read_csv("data/brand_categories.csv")
  } else {
    brand_map <- build_brand_categories(mappings_dir)
  }
  
  df <- df %>%
    left_join(brand_map, by = "brand") %>%
    mutate(station_type = replace_na(station_type, "Altro"),
           brand_group = factor(brand_group),
           station_type = factor(station_type))
  
  if (isTRUE(test)) {
    df <- df %>% 
      filter(nuts1 == c("Centro"))
  }
  
  return(list(df = df, stations_sf = stations_sf))
}

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
# 3. ROLLING WINDOW EXECUTION
# ------------------------------------------------------------------------------
run_rolling_panel_models <- function(base_data, window_size = 52, radius_m = 30000, split_by_nuts1 = FALSE) {
  df_all <- base_data$df
  stations_all_sf <- base_data$stations_sf
  
  if (split_by_nuts1) {
    df_all <- df_all %>% filter(!is.na(nuts1))
    regions <- unique(df_all$nuts1)
  } else {
    regions <- "All"
  }
  
  unique_weeks <- sort(unique(df_all$time_index))
  num_windows <- length(unique_weeks) - window_size + 1
  results_list <- list()
  
  for (region in regions) {
    if (split_by_nuts1) {
      cat(sprintf("\n=== Processing nuts1 Region: %s ===\n", region))
      df_region <- df_all %>% filter(nuts1 == region)
    } else {
      df_region <- df_all
    }
    
    region_results <- list()
    
    for (i in seq_len(num_windows)) {
      window_weeks <- unique_weeks[i:(i + window_size - 1)]
      win_label <- paste(window_weeks[1], window_weeks[window_size], sep = "_to_")
      cat(sprintf("Processing Window %d/%d: %s\n", i, num_windows, win_label))
      
      df_window <- df_region %>% filter(time_index %in% window_weeks)
      complete_pumps <- df_window %>%
        filter(complete.cases(mean_price_gasoline_self, latitude, longitude, 
                              population_density_km2, brand_group, station_type)) %>%
        count(id_pump) %>% filter(n == window_size) %>% pull(id_pump)
      
      if (length(complete_pumps) == 0) {
        cat("  -> Skipped: No complete pumps in this window.\n")
        next
      }
      
      df_window <- df_window %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump)
      stations_window_sf <- stations_all_sf %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump)
      
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
        cat("  -> Skipped: Insufficient connected stations.\n")
        next
      }
      
      W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
                    style = "W", zero.policy = TRUE)


      # Convert to panel object
      df_panel <- pdata.frame(df_window, index = c("id_pump", "time_index"))
      
      # 1. Dynamically build formula to avoid contrast errors
      n_brand <- length(unique(as.character(df_window$brand_group)))
      n_station <- length(unique(as.character(df_window$station_type)))
      
      features <- c(
        "lag(mean_price_gasoline_self, 1)", 
        "lag(brent_diff_pos, 1)", 
        "lag(brent_diff_neg, 1)", 
        "population_density_km2"
      )
      
      if (n_brand > 1) features <- c(features, "brand_group")
      if (n_station > 1) features <- c(features, "station_type")
      
      dynamic_formula <- as.formula(paste("mean_price_gasoline_self ~", paste(features, collapse = " + ")))
      
      # 2. Run Models using the dynamic_formula
      window_models <- list()
      for (m_name in names(models_spec)) {
        spec <- models_spec[[m_name]]
        tryCatch({
          window_models[[m_name]] <- spgm(
            dynamic_formula, # Use the dynamically generated formula here
            data = df_panel, listw = W, 
            lag = spec$lag, spatial.error = spec$spatial.error, Durbin = spec$Durbin, method = "w2sls"
          )
        }, error = function(e) cat(sprintf("  -> %s failed: %s\n", m_name, e$message)))
      }
      

      
      region_results[[win_label]] <- list(
        models = window_models,
        end_week = window_weeks[window_size],
        n_stations = nrow(stations_window_sf)
      )
    }
    
    results_list[[region]] <- region_results
  }
  
  return(list(results = results_list, split_by_nuts1 = split_by_nuts1))
}


# ------------------------------------------------------------------------------
# 4. DIAGNOSTICS & COMPARISONS (UPDATED)
# ------------------------------------------------------------------------------
analyze_comparative_diagnostics <- function(rolling_output) {
  rolling_results <- rolling_output$results
  is_split <- rolling_output$split_by_nuts1 
  
  coef_df <- map_dfr(names(rolling_results), function(region_name) {
    region_data <- rolling_results[[region_name]]
    map_dfr(names(region_data), function(win) {
      mod_data <- region_data[[win]]
      map_dfr(names(mod_data$models), function(m_name) {
        summ <- summary(mod_data$models[[m_name]])
        c_df <- as.data.frame(summ$CoefTable)
        colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
        c_df$term <- rownames(c_df)
        c_df$model <- m_name
        c_df$window <- win
        c_df$end_week <- mod_data$end_week
        c_df$nuts1 <- region_name
        c_df$date <- as.Date(paste0(mod_data$end_week, "-1"), format = "%Y-%W-%u")
        return(c_df)
      })
    })
  })
  rownames(coef_df) <- NULL
  
  # Use grepl for robust matching ignoring spaces
  coef_df <- coef_df %>% mutate(term = case_when(
    grepl("lambda|rho", term, ignore.case = TRUE) ~ "Spatial Lag (rho)",
    grepl("mean_price_gasoline_self", term) ~ "Time Lag (Gasoline)",
    grepl("brent_price_mean", term) ~ "Time Lag (Brent)",
    grepl("brent_diff_pos", term) ~ "Time Lag (Brent Δ+)",
    grepl("brent_diff_neg", term) ~ "Time Lag (Brent Δ-)",
    TRUE ~ term
  ))
  
  plot_comparative <- function(target_term, title) {
    plot_data <- coef_df %>% filter(term == target_term)
    
    # Safeguard against missing terms
    if (nrow(plot_data) == 0) {
      warning(paste("No data found for term:", target_term, "- Check model output names."))
      return(ggplot() + theme_void() + labs(title = paste("Missing data for", target_term)))
    }
    
    p <- ggplot(plot_data, aes(x = date, y = estimate, color = model)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_line(linewidth = 0.8) +
      theme_minimal() +
      labs(title = title, x = "Date", y = "Estimate", color = "Model")
    
    if (is_split) {
      p <- p + facet_wrap(~ nuts1, scales = "free_y") +
        labs(subtitle = "Faceted by nuts1 Region")
    }
    return(p)
  }
  
  p1 <- plot_comparative("Spatial Lag (rho)", "Comparison of Spatial Lag Estimates")
  p2 <- plot_comparative("Time Lag (Brent Δ+)", "Comparison of Positive Brent Price Change Transmission")
  
  print(p1)
  print(p2)
  
  return(list(stats_data = coef_df, plots = list(p1, p2)))
}

# ------------------------------------------------------------------------------
# 5. MAP GENERATION FUNCTIONS
# ------------------------------------------------------------------------------

# 5A. Update Summary Function to Group by Region
generate_summary_table <- function(stats_data) {
  require(dplyr)
  
  summary_table <- stats_data %>%
    # Group by nuts1 alongside term and model to retain spatial granularity
    group_by(nuts1, term, model) %>%
    summarize(
      mean_estimate = mean(estimate, na.rm = TRUE),
      median_estimate = median(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      mean_p_value = mean(p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(nuts1, term, model)
  
  return(summary_table)
}

# 5B. Map Plotting Function
plot_coefficient_maps <- function(summary_table, shp_path, nuts_path, 
                                  target_model = "SDM", 
                                  target_term = "Spatial Lag (rho)") {
  require(sf)
  require(dplyr)
  require(ggplot2)
  require(jsonlite)
  
  # 1. Load and prepare geographic bounds
  nuts_data <- as.data.frame(fromJSON(nuts_path)$resultset)
  
  nuts_data <- nuts_data %>% 
    select(PRO_COM, contains("DEN")) %>% 
    rename(
      nuts1 = DEN_RIP,
      nuts2 = DEN_REG,
      nuts3 = DEN_UTS
    )
  
  # Ensure the join uses the correct municipal ID column matching your JSON and SHP
  shp_data <- st_read(shp_path, quiet = TRUE) %>%
    left_join(nuts_data, by = c("PRO_COM" = "PRO_COM"))
  
  # 2. Aggregate municipal geometries into nuts1 regional boundaries
  # Note: Adjust 'nuts1' here if your JSON mapping script named it differently
  region_polygons <- shp_data %>%
    group_by(nuts1) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  # 3. Filter statistical data for the requested map layer
  stats_subset <- summary_table %>%
    filter(model == target_model, term == target_term)
  
  # 4. Join geometries with the model estimates
  map_data <- region_polygons %>%
    left_join(stats_subset, by = "nuts1")
  
  # 5. Generate Choropleth Map
  p_map <- ggplot(data = map_data) +
    geom_sf(aes(fill = mean_estimate), color = "white", linewidth = 0.3) +
    scale_fill_viridis_c(option = "magma", na.value = "grey80") +
    theme_void() +
    labs(
      title = sprintf("Geographic Distribution: %s", target_term),
      subtitle = sprintf("Model Specification: %s | Aggregation: nuts1", target_model),
      fill = "Mean\nEstimate"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right"
    )
  
  print(p_map)
  return(list(plot = p_map, map_data = map_data))
}

# ------------------------------------------------------------------------------
# 6. PIPELINE WRAPPER AND EXECUTION SCRIPT
# ------------------------------------------------------------------------------

# 6A. Update plot_summary_statistics to handle nuts1 facets
plot_summary_statistics <- function(summary_table) {
  require(ggplot2)
  require(dplyr)
  
  plot_data <- summary_table %>% filter(term != "(Intercept)")
  
  p <- ggplot(plot_data, aes(y = reorder(model, mean_estimate), x = mean_estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_linerange(aes(xmin = mean_estimate - sd_estimate, 
                       xmax = mean_estimate + sd_estimate), 
                   linewidth = 1.2, alpha = 0.6) +
    geom_point(size = 3.5) +
    geom_point(aes(x = median_estimate), size = 1.5, shape = 21, fill = "white", stroke = 1) +
    theme_minimal() +
    labs(
      title = "Coefficient Stability and Central Tendency",
      subtitle = "Solid dot = Mean | White dot = Median | Lines = ±1 SD",
      x = "Coefficient Estimate",
      y = "Model Specification"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11),
      axis.text.y = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank()
    )
  
  # Facet dynamically depending on whether nuts1 is present
  if ("nuts1" %in% colnames(plot_data) && length(unique(plot_data$nuts1)) > 1) {
    p <- p + facet_grid(nuts1 ~ term, scales = "free_x")
  } else {
    p <- p + facet_wrap(~ term, scales = "free_x", ncol = 2)
  }
  
  print(p)
  return(p)
}

# 6B. Main Pipeline Function
run_spatial_pipeline <- function(df_file, stations_csv, pop_dir, shp_path, nuts_path, rdata_dir, output_dir, window_weeks = 52, radius_m = 30000, split_by_nuts1 = FALSE, test = FALSE) {
  
  cat("Starting Data Preparation...\n")
  base_data <- prepare_integrated_panel(
    data_file          = df_file,
    stations_list_file = stations_csv,
    pop_dir            = pop_dir,
    shp_path           = shp_path,
    mappings_dir       = rdata_dir,
    test               = test
    # Note: Ensure prepare_integrated_panel handles nuts_path correctly per your prior updates
  )
  cat("Data Preparation Complete.\n\n")
  
  cat(sprintf("Starting Rolling Panel Models (%d-week windows)...\n", window_weeks))
  rolling_output <- run_rolling_panel_models(
    base_data      = base_data,
    window_size    = window_weeks,
    radius_m       = radius_m,
    split_by_nuts1 = split_by_nuts1
  )
  cat("Model Execution Complete.\n\n")
  
  cat("Starting Diagnostics and Analysis...\n")
  diagnostics <- analyze_comparative_diagnostics(rolling_output)
  cat("Analysis Complete.\n\n")
  
  # Save Outputs
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  saveRDS(rolling_output, file.path(output_dir, "rolling_models_output.RDS"))
  write_csv(diagnostics$stats_data, file.path(output_dir, "comparative_model_stats.csv"))
  
  ggsave(file.path(output_dir, "spatial_lag_comparison.png"), plot = diagnostics$plots[[1]], width = 12, height = 8)
  ggsave(file.path(output_dir, "brent_lag_comparison.png"), plot = diagnostics$plots[[2]], width = 12, height = 8)
  
  # Summary statistics and plots
  summary_stats <- generate_summary_table(diagnostics$stats_data)
  write_csv(summary_stats, file.path(output_dir, "summary_statistics.csv"))
  
  p_summary <- plot_summary_statistics(summary_stats)
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
# 7. EXECUTION
# ------------------------------------------------------------------------------

# Define Input and Output Paths
df_file      <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/part_0.RDS"
rdata_dir    <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly"
stations_csv <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
pop_dir      <- "/Volumes/T7 Shield/FRES/fuels_data/population"
shp_path     <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"
nuts_path    <- "/Volumes/T7 Shield/FRES/fuels_data/reportspooljson.json"
output_dir   <- "/Volumes/T7 Shield/FRES/fuels_data/results"

# Define Execution Parameters
window_weeks   <- 52
radius_m       <- 30000
split_by_nuts1 <- TRUE # Set to TRUE to run regional analysis and generate maps
test <- FALSE

# Execute Pipeline
pipeline_results <- run_spatial_pipeline(
  df_file        = df_file,
  stations_csv   = stations_csv,
  pop_dir        = pop_dir,
  shp_path       = shp_path,
  nuts_path      = nuts_path,
  rdata_dir      = rdata_dir,
  output_dir     = output_dir,
  window_weeks   = window_weeks,
  radius_m       = radius_m,
  split_by_nuts1 = split_by_nuts1,
  test           = test
)

