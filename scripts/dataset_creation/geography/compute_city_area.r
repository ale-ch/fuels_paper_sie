library(sf)
library(units)
library(dplyr)

path <- '/Volumes/T7 Shield/FRES/fuels_data/city_geom/Limiti01012026_g/Com01012026_g/Com01012026_g_WGS84.shp'

# 1. Read the shapefile
shp_data <- st_read(path)
shp_data$COMUNE <- str_replace_all(
  toupper(shp_data$COMUNE), 
  c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'")
)
shp_data <- shp_data %>%
  mutate(
    COMUNE = ifelse(
      !is.na(COMUNE_A) & COMUNE_A != "", 
      sub("/.*", "", COMUNE), 
      COMUNE
    )
  )

shp_data$area_sq_km <- shp_data$Shape_Area / 1000000

shp_data <- shp_data %>% 
  rename(
    city = COMUNE
  ) %>% 
  select(
    city, area_sq_km
  ) %>% 
  st_drop_geometry()


base_data$df$city <- str_replace_all(
  toupper(base_data$df$city), 
  c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'")
)


# 2. Join the shapefile geometry to your existing data frame
combined_data <- left_join(base_data$df, shp_data, by = "city")

