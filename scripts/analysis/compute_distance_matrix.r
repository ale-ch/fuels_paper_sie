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
    !is.na(stations_list$latitude), !is.na(stations_list$longitude)
  ) %>% 
  st_as_sf(coords=c("longitude", "latitude")) %>% 
  st_set_crs(4326)



stations_sample <- stations_list %>% filter(province == "RM")
stations_sample <- stations_sample[sample(nrow(stations_sample), 500, replace = FALSE), ]


get_knn <- function(stations_list, k = 10) {
  # Ensure geometry is projected to a metric CRS (e.g., EPSG:32632 for Italy) to calculate meters
  stations_list_sf <- st_transform(stations_list, crs = 32632)
  coords <- st_coordinates(stations_list_sf)
  
  # A. Calculate Min, Mean, Max Euclidean Distance to other stations
  # Using k=10 to approximate local neighborhood distances for computational efficiency
  start_time <- Sys.time()
  stations_knn <- get.knn(coords, k = k)
  end_time <- Sys.time()
  end_time - start_time
  
  list(
    data = stations_list_sf, 
    nn_index = stations_knn$nn.index,
    coords = coords
    )
}

stations_knn <- get_knn(stations_sample, 50)

stations_list_sf <- stations_knn$data
nn_index <- stations_knn$nn_index
coords <- stations_knn$coords

radius_m <- 30000
n_obs <- nrow(nn_index)

# Initialize the output list
filtered_nn_list <- vector("list", n_obs)
dists_list <- vector("list", n_obs)

for (i in 1:n_obs) {
  # Calculate distances from target station 'i' to its k neighbors
  dists <- as.numeric(st_distance(
    stations_list_sf[i, ], 
    stations_list_sf[nn_index[i, ], ]
  ))
  
  # Filter indices based on the 30km threshold
  valid_mask <- dists <= radius_m
  dists_list[[i]] <- dists
  filtered_nn_list[[i]] <- nn_index[i, valid_mask]
}
