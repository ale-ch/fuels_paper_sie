library(foreach)
library(doParallel)
library(dplyr)
library(tidyverse)
library(sf)
library(spdep)
library(plm)
library(splm)


prepare_window_data <- function(df, start_week, window_size) {
  end_week <- start_week + window_size - 1
  window_df <- df %>% filter(time_index >= start_week, time_index <= end_week)
  
  if (nrow(window_df) == 0) return(NULL)
  
  complete_pumps <- window_df %>%
    filter(complete.cases(mean_price_gasoline_self, latitude, longitude), !is.nan(mean_price_gasoline_self)) %>%
    count(id_pump) %>% filter(n == window_size) %>% pull(id_pump)
  
  if (length(complete_pumps) < 10) return(NULL)
  
  window_df <- window_df %>% filter(id_pump %in% complete_pumps) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32632)
  window_df_unique <- window_df %>% group_by(id_pump) %>% slice(1) %>% ungroup()
  
  return(list(df = window_df, df_unique = window_df_unique))
}

compute_distance_matrix <- function(window_data, radius_m) {
  df <- window_data$df
  df_unique <- window_data$df_unique
  
  coords <- st_coordinates(df_unique)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
  
  isolated <- which(card(nb) == 0)
  if (length(isolated) > 0) df_unique <- df_unique[-isolated, ]
  if (nrow(df_unique) < 10) return(NULL)
  
  df <- df %>% filter(id_pump %in% df_unique$id_pump) %>% arrange(id_pump, time_index)
  df_unique <- df_unique %>% arrange(id_pump)
  
  coords <- st_coordinates(df_unique)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
  
  W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
                style = "W", zero.policy = TRUE)
  return(list(df = df, W = W))
}



base_data <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/data/dataset.RDS")

region_df <- base_data$df %>% filter(nuts3 == "Milano")

window_df <- region_df %>% filter(time_index >= "2022-01", time_index <= "2022-52")

complete_pumps <- window_df %>%
  filter(complete.cases(mean_price_gasoline_self, latitude, longitude), !is.nan(mean_price_gasoline_self)) %>%
  count(id_pump) %>% filter(n == 52) %>% pull(id_pump)

if (length(complete_pumps) < 10) return(NULL)

window_df <- window_df %>% filter(id_pump %in% complete_pumps) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(32632)
window_df_unique <- window_df %>% group_by(id_pump) %>% slice(1) %>% ungroup()


df <- window_df
df_unique <- window_df_unique

coords <- st_coordinates(df_unique)
nb <- dnearneigh(coords, d1 = 0, d2 = 15000, longlat = FALSE)

isolated <- which(card(nb) == 0)
if (length(isolated) > 0) df_unique <- df_unique[-isolated, ]
if (nrow(df_unique) < 10) return(NULL)

df <- df %>% filter(id_pump %in% df_unique$id_pump) %>% arrange(id_pump, time_index)
df_unique <- df_unique %>% arrange(id_pump)

coords <- st_coordinates(df_unique)
nb <- dnearneigh(coords, d1 = 0, d2 = 15000, longlat = FALSE)

W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
              style = "W", zero.policy = TRUE)


p_data <- pdata.frame(df, index = c("id_pump", "time_index"))

# 1. Get the single integer for the number of time periods (e.g., 52)
t_periods <- length(unique(df$time_index))

# 2. Fix the formula typo (pos + neg)
fit <- spgm(mean_price_gasoline_self ~ brent_diff_pos + brent_diff_neg, 
            data = p_data, listw = W, model = "within", method = "w2sls",
            lag = TRUE, spatial.error = FALSE, Durbin = FALSE)

# 3. Pass the integer 't_periods' to the time argument
exact_impacts <- spatialreg::impacts(
  fit, 
  listw = W, 
  time = t_periods
)

print(exact_impacts)



# Extract coefficients vector from the fitted spgm model
coefs <- fit$coefficients

# Isolate lambda and the time-varying betas
lambda <- coefs["lambda"]
beta_pos <- coefs["brent_diff_pos"]
beta_neg <- coefs["brent_diff_neg"]

# Compute approximated impacts
approx_impacts <- data.frame(
  Term = c("brent_diff_pos", "brent_diff_neg"),
  Direct_Impact = c(beta_pos, beta_neg),
  Total_Impact = c(beta_pos / (1 - lambda), beta_neg / (1 - lambda))
)

# Calculate indirect impacts
approx_impacts$Indirect_Impact <- approx_impacts$Total_Impact - approx_impacts$Direct_Impact

print(approx_impacts)
