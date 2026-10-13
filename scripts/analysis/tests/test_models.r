# ------------------------------------------------------------------------------
# QUICK SAMPLE & ESTIMATE SCRIPT (USING PIPELINE FUNCTION)
# ------------------------------------------------------------------------------
library(plm)
library(splm)
library(spdep)
library(dplyr)


base_path <- "/Volumes/T7 Shield/FRES/repos/fuels_paper_sie"
setwd(base_path)


rdata_dir       <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly"
stations_list_file    <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"
pop_dir         <- "/Volumes/T7 Shield/FRES/fuels_data/population"
shp_path        <- "/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp"
nuts_path       <- "/Volumes/T7 Shield/FRES/fuels_data/reportspooljson.json"
mappings_dir    <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly" 
base_output_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/runs"


base_data <- prepare_integrated_panel(rdata_dir, stations_list_file, pop_dir, shp_path, mappings_dir, nuts_path, 
                                     min_stations = 30, sample_fraction = 0.10, test = FALSE, 
                                     scope_level = "nuts2", scope_name = "Lombardia", seed = 42)


df_sample <- base_data$df %>% filter(year == 2023)
window_size <- 53

complete_pumps <- df_sample %>%
  filter(complete.cases(mean_price_gasoline_self, latitude, longitude, population_density_km2, brand_group, station_type)) %>%
  count(id_pump) %>% filter(n == window_size) %>% pull(id_pump)

df_sample <- df_sample %>% filter(id_pump %in% complete_pumps) %>% arrange(id_pump, time_index)
stations_sf <- base_data$stations_sf %>% filter(id_pump %in% df_sample$id_pump) %>% arrange(id_pump)

# 4. SPATIAL WEIGHTS MATRIX
radius_m <- 15000
coords <- st_coordinates(stations_sf)
nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)

isolated <- which(card(nb) == 0)
if (length(isolated) > 0) {
  stations_sf <- stations_sf[-isolated, ]
  df_sample <- df_sample %>% filter(id_pump %in% stations_sf$id_pump)
  coords <- st_coordinates(stations_sf)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
}

W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
              style = "W", zero.policy = TRUE)


# 5. ESTIMATION (SDM Example)
cat("Estimating Model...\n")
p_data <- pdata.frame(df_sample, index = c("id_pump", "time_index"))


fit <- spgm(
  mean_price_gasoline_self ~ population_density_km2 + brand_group + brent_diff_pos + brent_diff_neg, 
  data = p_data, 
  listw = W, 
  lag = TRUE, 
  spatial.error = TRUE, 
  Durbin = TRUE, 
  method = "w2sls"
)

summary(fit)



fit <- spgm(
  mean_price_gasoline_self ~ population_density_km2 + brand_group + brent_diff_pos + brent_diff_neg, 
  data = p_data, 
  listw = W, 
  lag = TRUE, 
  spatial.error = FALSE, 
  Durbin = TRUE, 
  method = "w2sls"
)

summary(fit)




# THESE COMBINATIONS RETURN ERROR!!! NO ENDOGENOUS VARIABLES

fit <- spgm(
  mean_price_gasoline_self ~ population_density_km2 + brand_group + brent_diff_pos + brent_diff_neg, 
  data = p_data, 
  listw = W, 
  lag = FALSE, 
  spatial.error = FALSE, 
  Durbin = FALSE, 
  method = "w2sls"
)

summary(fit)


fit <- spgm(
  mean_price_gasoline_self ~ population_density_km2 + brand_group + brent_diff_pos + brent_diff_neg, 
  data = p_data, 
  listw = W, 
  lag = FALSE, 
  spatial.error = FALSE, 
  Durbin = TRUE, 
  method = "w2sls"
)

summary(fit)


