rdata_dir       <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly"
stations_list_file    <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
pop_dir         <- "/Volumes/T7 Shield/FRES/fuels_data/population"
shp_path        <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"
nuts_path       <- "/Volumes/T7 Shield/FRES/fuels_data/reportspooljson.json"
mappings_dir    <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly" 
base_output_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results"


prepare_panel_data <- function(rdata_dir, stations_list_file, pop_dir, shp_path, mappings_dir, nuts_path) {
  # 1. Load and spatially join stations to NUTS regions for sampling
  cat("Loading geographic boundaries for spatial sampling...\n")
  nuts_data <- as.data.frame(fromJSON(nuts_path)$resultset)
  shp_data <- st_read(shp_path, quiet = TRUE) %>%
    mutate(
      area_sq_km = Shape_Area / 1000000
    ) %>% 
    left_join(nuts_data %>% select(PRO_COM, DEN_RIP, DEN_REG, DEN_UTS), by = c("PRO_COM")) %>%
    select(nuts1 = DEN_RIP, nuts2 = DEN_REG, nuts3 = DEN_UTS, city = COMUNE, area_sq_km, geometry) %>% # Keep all 3 levels for dynamic filtering
    filter(!duplicated(city)) %>% 
    st_transform(32632)
  
  shp_data$city <- str_replace_all(toupper(shp_data$city), 
                                   c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'"))
  
  cat("Loading and spatially joining stations...\n")
  stations_sf <- read_delim(stations_list_file, delim = "|", show_col_types = FALSE) %>%
    select(-c(address, contains("Estrazione"))) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(32632) %>% arrange(id_pump) 
  
  stations_sf <- stations_sf %>% st_drop_geometry() %>% 
    left_join(shp_data, by = "city") %>%
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
    left_join(stations_sf %>% select(id_pump, area_sq_km, contains("nuts")), by = "id_pump")
  
  population_data <- build_population_data(pop_dir, overwrite = FALSE)
  
  df_joined <- df_joined %>% left_join(population_data)
  
  df_joined <- df_joined %>% 
    mutate(
      population_density = population / area_sq_km
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
  
  
  return(list(df = df, stations_sf = stations_sf, failed_files = failed_files))
}

base_data <- prepare_panel_data(rdata_dir, stations_list_file, pop_dir, shp_path, mappings_dir, nuts_path)


