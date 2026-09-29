require(tidyverse)
require(sf)
require(spdep)
require(FNN)
require(plm)
require(splm)
require(quantmod)
require(zoo)
require(lubridate)

source("prepare_spatial_panel.r")

file_path <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/part_0.RDS"
stations_list_file <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
k <- 10
target_year <- 2021

results <- prepare_spatial_panel(data_file = file_path, stations_list = stations_list_file, k = k, target_year = target_year)

df_panel <- results$df
stations <- results$stations
W <- results$W

# Model 1: SAR (Spatial Autoregressive)
model_sar <- spgm(
  mean_price_gasoline_self ~ lag(mean_price_gasoline_self, 1),
  data = df_panel, 
  listw = W, 
  lag = TRUE, 
  Durbin = TRUE, 
  spatial.error = FALSE, 
  method = "w2sls"
)

summary(model_sar)
