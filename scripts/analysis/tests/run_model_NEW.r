base_data <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/dataset.RDS")

# base_data$df <- base_data$df %>% filter(nuts1 == "Nord-ovest")
# base_data$stations_sf <- base_data$stations_sf %>% filter(nuts1 == "Nord-ovest")


# 1. Ensure strictly balanced panel
complete_pumps <- base_data$df %>%
  filter(
    year == 2022, 
    complete.cases(mean_price_gasoline_self, latitude, longitude),
    !is.nan(mean_price_gasoline_self)
  ) %>%
  count(id_pump) %>% 
  filter(n == 53) %>% 
  pull(id_pump)


df <- base_data$df %>% 
  filter(id_pump %in% complete_pumps, year == 2022) %>% 
  st_as_sf(
    coords = c("longitude", "latitude"), 
    crs = 4326
  )  %>% 
  st_transform(32632)

df_unique <- df %>% 
  group_by(id_pump) %>% 
  slice(1) %>% 
  ungroup()

coords <- st_coordinates(df_unique)
radius_m <- 30000
nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
isolated <- which(card(nb) == 0)
if (length(isolated) > 0) {
  df_unique <- df_unique[-isolated, ]
}

# 1. Align the main panel data by dropping the isolated stations
df <- df %>% filter(id_pump %in% df_unique$id_pump)

# 2. Strictly sort both objects by ID so W and p_data align perfectly
df_unique <- df_unique %>% arrange(id_pump)
df <- df %>% arrange(id_pump, time_index)

# 3. Recompute W with the aligned, sorted geometries
coords <- st_coordinates(df_unique)
nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
              style = "W", zero.policy = TRUE)

p_data <- pdata.frame(df, index = c("id_pump", "time_index"))

# 4. Set Durbin = FALSE for macroeconomic variables
fit <- spgm(mean_price_gasoline_self ~ brent_diff_pos + brent_diff_neg, 
            data = p_data, listw = W, lag = TRUE, model = "within",
            spatial.error = FALSE, Durbin = FALSE, method = "w2sls")

