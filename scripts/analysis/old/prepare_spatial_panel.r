prepare_spatial_panel <- function(data_file, stations_list, k = 10, target_year = 2020) {
  
  # Load required packages
  require(tidyverse)
  require(sf)
  require(spdep)
  require(FNN)
  require(plm)
  require(splm)
  require(quantmod)
  require(zoo)
  require(lubridate)
  
  # 1. Read Data
  df <- readRDS(data_file)
  
  if (is.character(stations_list)) {
    stations_list <- read_delim(stations_list, delim = "|", show_col_types = FALSE)
  }
  
  # Clean stations list
  stations_list <- stations_list %>% 
    select(-c(address, contains("Estrazione"))) %>% 
    filter(!is.na(latitude), !is.na(longitude)) %>% 
    st_as_sf(coords = c("longitude", "latitude")) %>% 
    st_set_crs(4326)
  
  # 2. Filter Balanced Panel for Target Year
  expected_weeks <- df %>% filter(year == target_year) %>% distinct(week) %>% nrow()
  
  full_panel_idx <- df %>% 
    group_by(year, id_pump) %>% 
    count() %>% 
    filter(year == target_year, n == expected_weeks) %>% 
    ungroup() %>% 
    select(id_pump)
  
  df <- df %>% 
    filter(
      !is.na(latitude), 
      !is.na(longitude), 
      id_pump %in% full_panel_idx$id_pump,
      year == target_year
    ) %>% 
    group_by(id_pump) %>%
    mutate(
      mean_price_gasoline_self = ifelse(
        is.na(mean_price_gasoline_self), 
        mean(mean_price_gasoline_self, na.rm = TRUE),
        mean_price_gasoline_self
      )
    ) %>%
    filter(!is.nan(mean_price_gasoline_self)) %>% 
    ungroup() %>% 
    st_as_sf(coords = c("longitude", "latitude")) %>% 
    st_set_crs(4326)
  
  # 3. Setup Spatial Sample
  stations_sample_idx <- df %>% 
    filter(!duplicated(id_pump)) %>% 
    pull(id_pump)
  
  stations_sample <- stations_list %>% 
    filter(id_pump %in% stations_sample_idx)
  
  stations_sample_sf <- st_transform(stations_sample, crs = 32632) %>% arrange(id_pump)
  
  # 4. Geospatial Distances (k-NN and 30km radius)
  coords <- st_coordinates(stations_sample_sf)
  
  knn_dist <- get.knn(coords, k = k)$nn.dist
  stations_sample_sf$dist_min <- apply(knn_dist, 1, min)
  stations_sample_sf$dist_mean <- apply(knn_dist, 1, mean)
  stations_sample_sf$dist_max <- apply(knn_dist, 1, max)
  
  radius_30km <- 30000 
  nb_30km <- dnearneigh(coords, d1 = 0, d2 = radius_30km)
  stations_sample_sf$n_stations_30km <- card(nb_30km)
  
  # 5. Remove Isolated Stations
  isolated_pumps <- stations_sample_sf %>% 
    filter(n_stations_30km == 0) %>% 
    pull(id_pump)
  
  stations_sample_sf <- stations_sample_sf %>%
    filter(!(id_pump %in% isolated_pumps)) %>%
    arrange(id_pump) 
  
  df <- df %>%
    filter(!(id_pump %in% isolated_pumps)) %>% 
    arrange(id_pump)
  
  # 6. Rebuild Spatial Weights Matrix (W)
  coords_clean <- st_coordinates(stations_sample_sf)
  nb_clean <- dnearneigh(coords_clean, d1 = 0, d2 = radius_30km)
  dist_list_clean <- nbdists(nb_clean, coords_clean)
  inv_dist_clean <- lapply(dist_list_clean, function(x) ifelse(x > 0, 1 / x, 0))
  W <- nb2listw(nb_clean, glist = inv_dist_clean, style = "W", zero.policy = TRUE)
  
  # 7. Fetch Brent Crude Data
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
  
  # 8. Merge and Prepare Panel
  df_sf <- left_join(
    df, 
    stations_sample_sf %>% 
      st_drop_geometry() %>% 
      select(id_pump, contains("dist"), n_stations_30km),
    by = "id_pump"
  )
  
  df_sf <- left_join(df_sf, brent_weekly_df, by = c("year", "week"))
  df_clean <- st_drop_geometry(df_sf)
  
  df_clean <- df_clean %>%
    mutate(time_index = paste(year, sprintf("%02d", week), sep = "-"))
  
  df_panel <- pdata.frame(df_clean, index = c("id_pump", "time_index"))
  
  # 9. Return Objects
  return(list(
    df = df_panel,
    stations = stations_sample_sf,
    W = W
  ))
}

# Usage:

#file_path <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/part_0.RDS"
#stations_list_file <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
#k <- 10
#target_year <- 2020
#results <- prepare_spatial_panel(data_file = file_path, stations_list = stations_list_file, k = k, target_year = target_year)
# df_panel <- results$df
# stations <- results$stations
# W <- results$W