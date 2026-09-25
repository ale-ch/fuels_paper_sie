# ------------------------------------------------------------------------------
# 3. MERGE DATA AND PREPARE PANEL
# ------------------------------------------------------------------------------


df_sf <- left_join(df_sample_sf, 
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

# Pre-calculate spatial lag of Brent for Durbin/SLX variants using the new column name
df_panel$W_brent <- slag(df_panel$brent_price_mean, listw = W)

# Model 1: SAR (Spatial Autoregressive)
model_sar <- spgm(
  mean_price_gasoline_self ~ lag(mean_price_gasoline_self, 1) + brent_price_mean,
  data = df_panel, listw = W, lag = TRUE, 
  Durbin = TRUE, 
  spatial.error = FALSE, 
  method = "w2sls"
)






#### Working example ####
data(Produc, package="plm")
data(usaww)
usalw <- spdep::mat2listw(usaww)
fm <- log(gsp)~log(pcap)+log(pc)+log(emp)+unemp+slag(log(pcap),
                                                     listw=usalw)
slxmod <- spreml(fm, data=Produc, w = usaww,
                 model="pooling", lag=FALSE, errors="ols")


dim(usaww)
dim(Produc)

