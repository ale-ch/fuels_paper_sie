merge_shp_nuts <- function(nuts_path, shp_path) {
  nuts_data <- read_delim(nuts_path, delim = ";")
  nuts_data <- nuts_data %>% 
    select(
      nuts1 = NUTS1, nuts2 = NUTS2, nuts3 = NUTS3, PRO_COM = `Codice comune (numerico)`
    )
  
  shp_data <- st_read(shp_path, quiet = TRUE)
  
  diff_keys <- nuts_data$PRO_COM[which(!nuts_data$PRO_COM %in% shp_data$PRO_COM)]
  diff_keys2 <- shp_data$PRO_COM[which(!(shp_data$PRO_COM) %in% nuts_data$PRO_COM)]
  
  nuts_data <- nuts_data %>% 
    filter(!(PRO_COM %in% diff_keys))
  
  shp_data <- shp_data %>% 
    filter(!(PRO_COM %in% diff_keys2))
  
  shp_data <- st_read(shp_path, quiet = TRUE) %>%
    mutate(
      area_sq_km = Shape_Area / 1000000
    ) %>% 
    left_join(nuts_data, by = c("PRO_COM")) %>%
    select(nuts1, nuts2, nuts3, city = COMUNE, area_sq_km, geometry) %>% # Keep all 3 levels for dynamic filtering
    filter(!duplicated(city)) %>% 
    st_transform(32632) %>% 
    na.omit()
  
  shp_data$city <- str_replace_all(toupper(shp_data$city), 
                                   c("À" = "A'", "È" = "E'", "É" = "E'", "Ì" = "I'", "Ò" = "O'", "Ù" = "U'"))
  
  return(shp_data)
}
