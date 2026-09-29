# ------------------------------------------------------------------------------
# 2. GEOSPATIAL PREPARATION (EUCLIDEAN METRICS & SPATIAL WEIGHTS)
# ------------------------------------------------------------------------------


library(tidyverse)
library(sf)
library(spdep)
library(FNN)      # For fast Nearest Neighbor distances
library(dplyr)
library(plm)
library(splm)

file_path <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/part_0.RDS"

stations_list_file <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"


df <- readRDS(file_path)
stations_list <- read_delim(stations_list_file, delim = "|")

stations_list <- stations_list %>% 
  select(
    -c(address, contains("Estrazione"))
  ) %>% 
  filter(
    !is.na(latitude), !is.na(longitude)
  ) %>% 
  st_as_sf(coords=c("longitude", "latitude")) %>% 
  st_set_crs(4326)

# BALANCED 2020 PANEL 
full_2020_panel_idx <- df %>% 
  group_by(year, id_pump) %>% 
  count() %>% 
  filter(year == 2020, n == 53) %>% 
  ungroup() %>% 
  select(id_pump)

df <- df %>% 
  filter(
    !is.na(latitude), 
    !is.na(longitude), 
    id_pump %in% full_2020_panel_idx$id_pump,
    year == 2020
  ) %>% 
  group_by(id_pump) %>%
  # Impute missing values with id_pump temporal mean
  mutate(
    mean_price_gasoline_self = ifelse(
      is.na(mean_price_gasoline_self), 
      mean(mean_price_gasoline_self, na.rm = TRUE),
      mean_price_gasoline_self)) %>%
  filter(!is.nan(mean_price_gasoline_self)) %>% 
  ungroup() %>% 
  st_as_sf(coords=c("longitude", "latitude")) %>% 
  st_set_crs(4326)





#stations_sample_idx <- df %>% 
#  filter(!duplicated(id_pump), province == "RM") %>%
#  select(id_pump) %>% 
#  st_drop_geometry() %>% 
#  unlist()

# stations_sample <- stations_list %>% filter(province == "RM")
# stations_sample <- stations_sample[sample(nrow(stations_sample), 500, replace = FALSE), ]


stations_sample_idx <- df %>% 
  filter(!duplicated(id_pump)) %>% 
  select(id_pump) %>% 
  st_drop_geometry() %>% 
  unlist()
  

stations_sample <- stations_list %>% filter(id_pump %in% stations_sample_idx)

df_sample <- df %>% filter(id_pump %in% stations_sample_idx)


# Ensure geometry is in metric projection for Italy (UTM zone 32N)
stations_sample_sf <- st_transform(stations_sample, crs = 32632) %>% arrange(id_pump)
df_sample_sf <- st_transform(df_sample, crs = 32632) %>% arrange(id_pump)

coords <- st_coordinates(stations_sample_sf)

# A. Min, Mean, Max Euclidean distance to nearest stations (k = 10)
knn_dist <- get.knn(coords, k = 10)$nn.dist
stations_sample_sf$dist_min <- apply(knn_dist, 1, min)
stations_sample_sf$dist_mean <- apply(knn_dist, 1, mean)
stations_sample_sf$dist_max <- apply(knn_dist, 1, max)

# B. Station density within 30km Euclidean radius
radius_30km <- 30000 
nb_30km <- dnearneigh(coords, d1 = 0, d2 = radius_30km)
stations_sample_sf$n_stations_30km <- card(nb_30km)

# Find stations with 0 neighbors
isolated_pumps <- stations_sample_sf %>% 
  filter(n_stations_30km == 0) %>% 
  select(id_pump) %>% 
  unlist()

# Remove them from BOTH the spatial sample and the main panel data
stations_sample_sf <- stations_sample_sf %>%
  filter(!(id_pump %in% isolated_pumps)) %>%
  arrange(id_pump) # CRITICAL: Enforce alphanumeric sorting

df <- df %>%
  filter(!(id_pump %in% isolated_pumps)) %>% 
  arrange(id_pump)



# Re-extract coordinates from the newly sorted and filtered spatial data
coords_clean <- st_coordinates(stations_sample_sf)

# Re-run neighborhood and weights
nb_clean <- dnearneigh(coords_clean, d1 = 0, d2 = radius_30km)
dist_list_clean <- nbdists(nb_clean, coords_clean)
inv_dist_clean <- lapply(dist_list_clean, function(x) ifelse(x > 0, 1 / x, 0))
W <- nb2listw(nb_clean, glist = inv_dist_clean, style = "W", zero.policy = TRUE)



# C. Spatial Weight Matrix (Inverse Distance within 30km)
#dist_list <- nbdists(nb_30km, coords)
#inv_dist <- lapply(dist_list, function(x) ifelse(x > 0, 1 / x, 0))
#W <- nb2listw(nb_30km, glist = inv_dist, style = "W", zero.policy = TRUE)


# ------------------------------------------------------------------------------
# 1. FETCH DAILY BRENT CRUDE OIL DATA (2016-2026)
# ------------------------------------------------------------------------------
library(quantmod)
library(dplyr)
library(zoo)

# Fetch Brent daily prices from FRED (Federal Reserve Bank of St. Louis)
getSymbols("DCOILBRENTEU", src = "FRED", from = "2016-01-01", to = "2026-12-31")

library(tidyr)

brent_df <- data.frame(
  date = ymd(index(DCOILBRENTEU)),
  brent_price = as.numeric(DCOILBRENTEU$DCOILBRENTEU)
) %>%
  # 1. Generate a continuous daily sequence to create rows for missing days
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  # 2. Last Observation Carried Forward (imputes from previous day)
  mutate(brent_price = na.locf(brent_price, na.rm = FALSE))


# ------------------------------------------------------------------------------
# 3. MERGE DATA AND PREPARE PANEL
# ------------------------------------------------------------------------------


#df_sf <- left_join(df_sample_sf, 
#          stations_sample_sf %>% 
#            st_drop_geometry() %>% 
#            select(id_pump, contains("dist"), n_stations_30km))

df_sf <- left_join(df, 
                   stations_sample_sf %>% 
                     st_drop_geometry() %>% 
                     select(id_pump, contains("dist"), n_stations_30km))

brent_weekly_df <- brent_df %>% 
  group_by(year = year(date), week = week(date)) %>% 
  reframe(
    brent_price_mean = mean(brent_price, na.rm=TRUE)
  )


df_sf <- left_join(df_sf, brent_weekly_df, by = c("year", "week"))

df <- st_drop_geometry(df_sf)

# Create a unified time index (e.g., "2023-01", "2023-02") to ensure proper chronological sorting
df <- df %>%
  mutate(time_index = paste(year, sprintf("%02d", week), sep = "-"))

# Convert to plm panel object using the new identifiers
df_panel <- pdata.frame(df, index = c("id_pump", "time_index"))


rm(list=setdiff(ls(), list("df", "stations_sample_sf", "W")))
