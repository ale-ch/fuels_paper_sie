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

rdata_dir       <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly"
stations_list_file    <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
pop_dir         <- "/Volumes/T7 Shield/FRES/fuels_data/population"
shp_path        <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"
# nuts_path       <- "/Volumes/T7 Shield/FRES/fuels_data/reportspooljson.json"
nuts_path       <- "/Volumes/T7 Shield/FRES/fuels_data/nuts_data.csv"
mappings_dir    <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly" 
base_output_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results"

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

prepare_panel_data <- function(rdata_dir, stations_list_file, pop_dir, shp_path, mappings_dir, nuts_path) {
  # 1. Load and spatially join stations to NUTS regions for sampling
  cat("Loading geographic boundaries for spatial sampling...\n")
  
  nuts_data <- read_delim(nuts_path, delim = ";")
  nuts_data <- nuts_data %>% 
    select(
      nuts1 = NUTS1, nuts2 = NUTS2, nuts3 = NUTS3, PRO_COM = `Codice comune (numerico)`
    )
  
  shp_data <- st_read(shp_path, quiet = TRUE)
  
  diff_keys <- nuts_data$PRO_COM[which(!nuts_data$PRO_COM %in% shp_data$PRO_COM)]
  diff_keys2 <- shp_data$PRO_COM[which(!(shp_data$PRO_COM) %in% nuts_data$PRO_COM)]
  
  nuts_data <- nuts_data %>% 
    filter(!(PRO_COM %in% diff_keys))
  
  shp_data <- shp_data %>% 
    filter(!(PRO_COM %in% diff_keys2))
  
  shp_data <- st_read(shp_path, quiet = TRUE) %>%
    mutate(
      area_sq_km = Shape_Area / 1000000
    ) %>% 
    left_join(nuts_data, by = c("PRO_COM")) %>%
    select(nuts1, nuts2, nuts3, city = COMUNE, area_sq_km, geometry) %>% # Keep all 3 levels for dynamic filtering
    filter(!duplicated(city)) %>% 
    st_transform(32632) %>% 
    na.omit()
  
  shp_data$city <- str_replace_all(toupper(shp_data$city), 
                                   c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'"))
  
  
  cat("Loading and spatially joining stations...\n")
  stations_sf <- read_delim(stations_list_file, delim = "|", show_col_types = FALSE) %>%
    select(-c(address, province, contains("Estrazione"))) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(32632) %>% arrange(id_pump)
  
  stations_sf$city <- str_replace_all(toupper(stations_sf$city), 
                                      c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'"))
  
  stations_sf <- stations_sf %>% 
    st_drop_geometry() %>%
    left_join(shp_data , by = "city") %>%
    filter(!is.na(nuts3))
  
  
  
  # 4. Iteratively load panel data
  rds_files <- list.files(rdata_dir, pattern = "\\.RDS$", full.names = TRUE)
  cat(sprintf("Iteratively loading and filtering %d panel data files...\n", length(rds_files)))
  
  df <- tibble()
  failed_files <- character()
  
  for (file in rds_files) {
    tryCatch({
      df_part <- readRDS(file)
      df_filtered <- df_part #%>% filter(id_pump %in% sampled_ids)
      
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
  
  df_joined <- df %>% 
    filter(id_pump %in% stations_sf$id_pump) %>% 
    left_join(
      stations_sf %>% st_drop_geometry() %>% select(id_pump, contains("nuts"), area_sq_km), 
      by = "id_pump"
    )
  
  population_data <- build_population_data(pop_dir, overwrite = FALSE)
  
  df_joined <- df_joined %>% left_join(population_data)
  
  df_joined <- df_joined %>% 
    mutate(
      population_density_km2 = population / area_sq_km
    )
  
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
  
  
  df_joined <- df_joined %>%
    st_drop_geometry() %>% 
    left_join(brent_weekly_df, by = c("year", "week")) %>%
    mutate(time_index = paste(year, sprintf("%02d", week), sep = "-"),
           city = str_replace_all(toupper(city), c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'")))
  
  
  return(list(df = df_joined, stations_sf = stations_sf, failed_files = failed_files))
}

base_data <- prepare_panel_data(rdata_dir, stations_list_file, pop_dir, shp_path, mappings_dir, nuts_path)


# saveRDS(base_data, "/Volumes/T7 Shield/FRES/fuels_data/dataset.RDS")
