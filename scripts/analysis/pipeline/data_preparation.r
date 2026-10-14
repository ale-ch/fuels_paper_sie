# ------------------------------------------------------------------------------
# 1. DATA PREPARATION (UPDATED WITH STRATIFIED SAMPLING & REGIONAL FILTERING)
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

build_population_data <- function(base_dir, overwrite = FALSE) {
  if(!file.exists(file.path(base_dir, "city_population.csv")) | isTRUE(overwrite)) {
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
    
    pop_final <- pop_final %>% 
      group_by(city) %>% 
      reframe(
        year,
        population,
        n = n()
      ) %>% 
      filter(n <= 11) %>% 
      select(-n) %>% 
      ungroup()
    
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
  shp_data <- shp_data %>% left_join(nuts_data, by = "PRO_COM")
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

stratified_spatial_sample <- function(stations_sf, min_stations_per_region = 30, sample_fraction = 0.10, seed = 42) {
  set.seed(seed)
  sampled_stations <- stations_sf %>%
    group_by(nuts3) %>%
    mutate(
      n_total = n(),
      n_target = pmin(n_total, pmax(min_stations_per_region, round(n_total * sample_fraction)))
    ) %>%
    filter(row_number() %in% sample(row_number(), size = n_target[1])) %>%
    ungroup() %>%
    select(-n_total, -n_target)
  
  return(sampled_stations)
}

prepare_integrated_panel <- function(rdata_dir, stations_list_file, pop_dir, shp_path, mappings_dir, nuts_path, 
                                     min_stations = 30, sample_fraction = 0.10, test = FALSE, 
                                     scope_level = "all", scope_name = NULL, seed = 42) {
  
  set.seed(seed)
  
  # 1. Load and spatially join stations to NUTS regions for sampling
  cat("Loading geographic boundaries for spatial sampling...\n")
  nuts_data <- as.data.frame(fromJSON(nuts_path)$resultset)
  shp_data <- st_read(shp_path, quiet = TRUE) %>%
    left_join(nuts_data, by = c("PRO_COM" = "PRO_COM")) %>%
    select(nuts1 = DEN_RIP, nuts2 = DEN_REG, nuts3 = DEN_UTS, geometry) %>% # Keep all 3 levels for dynamic filtering
    st_transform(32632)
  
  cat("Loading and spatially joining stations...\n")
  stations_sf <- read_delim(stations_list_file, delim = "|", show_col_types = FALSE) %>%
    select(-c(address, contains("Estrazione"))) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(32632) %>% arrange(id_pump)
  
  stations_sf <- st_join(stations_sf, shp_data, join = st_intersects) %>%
    filter(!is.na(nuts3))
  
  # 2. Regional Filtering Block
  if (scope_level != "all") {
    cat(sprintf("Filtering stations strictly for scope %s: %s...\n", scope_level, scope_name))
    if (scope_level == "nuts1") {
      stations_sf <- stations_sf %>% filter(nuts1 %in% scope_name)
    } else if (scope_level == "nuts2") {
      stations_sf <- stations_sf %>% filter(nuts2 %in% scope_name)
    } else if (scope_level == "nuts3") {
      stations_sf <- stations_sf %>% filter(nuts3 %in% scope_name)
    }
    
    if (nrow(stations_sf) == 0) {
      stop(sprintf("Error: No stations found for %s = '%s'. Check your spelling in the main script.", 
                   scope_level, scope_name))
    }
  }
  
  # 3. Apply Stratified Sampling (Always uses nuts3 internally to ensure even spatial distribution)
  cat("Applying stratified spatial sampling...\n")
  stations_sf <- stratified_spatial_sample(
    stations_sf, 
    min_stations_per_region = min_stations, 
    sample_fraction = sample_fraction,
    seed = seed
  )
  sampled_ids <- stations_sf$id_pump
  cat(sprintf("Sampled %d stations across %d NUTS3 regions.\n", length(sampled_ids), length(unique(stations_sf$nuts3))))
  
  # 4. Iteratively load panel data
  rds_files <- list.files(rdata_dir, pattern = "\\.RDS$", full.names = TRUE)
  cat(sprintf("Iteratively loading and filtering %d panel data files...\n", length(rds_files)))
  
  df <- tibble()
  failed_files <- character()
  
  for (file in rds_files) {
    tryCatch({
      df_part <- readRDS(file)
      df_filtered <- df_part %>% filter(id_pump %in% sampled_ids)
      
      if (nrow(df) == 0) {
        df <- df_filtered
      } else {
        df <- bind_rows(df, df_filtered)
      }
    }, error = function(e) {
      cat(sprintf("  -> Skipped %s due to error: %s\n", basename(file), e$message))
      failed_files <<- c(failed_files, file)
    })
  }
  
  if (length(failed_files) > 0) {
    cat(sprintf("WARNING: %d files failed and were skipped.\n", length(failed_files)))
  }
  
  # 5. Fetch and clean daily Brent data
  cat("Merging macro-economic and demographic data...\n")
  getSymbols("DCOILBRENTEU", src = "FRED", from = "2016-01-01", to = "2026-12-31", auto.assign = TRUE)
  brent_df <- data.frame(
    date = ymd(index(DCOILBRENTEU)), 
    brent_price = as.numeric(DCOILBRENTEU$DCOILBRENTEU)
  ) %>%
    complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    mutate(brent_price = na.locf(brent_price, na.rm = FALSE))
  
  # 6. Aggregate to weekly, THEN calculate differences
  brent_weekly_df <- brent_df %>%
    group_by(year = year(date), week = week(date)) %>%
    reframe(brent_price_mean = mean(brent_price, na.rm = TRUE)) %>%
    arrange(year, week) %>%
    mutate(
      brent_diff = brent_price_mean - dplyr::lag(brent_price_mean),
      brent_diff_pos = replace_na(ifelse(brent_diff > 0, brent_diff, 0), 0),
      brent_diff_neg = replace_na(ifelse(brent_diff < 0, brent_diff, 0), 0)
    )
  
  # 7. Join the complete weekly data to the main panel
  df <- df %>%
    left_join(brent_weekly_df, by = c("year", "week")) %>%
    mutate(time_index = paste(year, sprintf("%02d", week), sep = "-"),
           city = str_replace_all(toupper(city), c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'")))
  
  # 8. Join demographics and region mappings
  df <- df %>%
    left_join(
      build_population_data(pop_dir) %>% 
        select(city, year, population), by = c("city", "year")
    ) %>%
    left_join(build_city_areas(shp_path, nuts_path), by = "city") %>%
    mutate(population_density_km2 = population / area_sq_km)
  
  # 9. Categorize brands
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
    if (scope_level == "all") {
      df <- df %>% filter(nuts1 == "Centro")
    }
  }
  
  return(list(df = df, stations_sf = stations_sf, failed_files = failed_files))
}