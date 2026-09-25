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

df <- df %>% 
  filter(
    !is.na(latitude), !is.na(longitude)
  ) %>% 
  st_as_sf(coords=c("longitude", "latitude")) %>% 
  st_set_crs(4326)


stations_sample <- stations_list %>% filter(province == "RM")
stations_sample <- stations_sample[sample(nrow(stations_sample), 500, replace = FALSE), ]

df_sample <- df %>% 
  filter(
    id_pump %in% stations_sample$id_pump
  )


# Ensure geometry is in metric projection for Italy (UTM zone 32N)
stations_sample_sf <- st_transform(stations_sample, crs = 32632)
df_sample_sf <- st_transform(df_sample, crs = 32632)

coords <- st_coordinates(stations_sample_sf)

# A. Min, Mean, Max Euclidean distance to nearest stations (k = 10)
knn_dist <- get.knn(coords, k = 30)$nn.dist
stations_sample_sf$dist_min <- apply(knn_dist, 1, min)
stations_sample_sf$dist_mean <- apply(knn_dist, 1, mean)
stations_sample_sf$dist_max <- apply(knn_dist, 1, max)

# B. Station density within 30km Euclidean radius
radius_30km <- 30000 
nb_30km <- dnearneigh(coords, d1 = 0, d2 = radius_30km)
stations_sample_sf$n_stations_30km <- card(nb_30km)

# C. Spatial Weight Matrix (Inverse Distance within 30km)
dist_list <- nbdists(nb_30km, coords)
inv_dist <- lapply(dist_list, function(x) ifelse(x > 0, 1 / x, 0))
W <- nb2listw(nb_30km, glist = inv_dist, style = "W", zero.policy = TRUE)


list(df_sample_sf, stations_sample_sf, W)


