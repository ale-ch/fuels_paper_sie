library(tidyverse)

files_list <- list.files("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly")
files_list <- files_list[unlist(lapply(files_list, function(x) {str_ends(x, "RDS")}))]


#### Categorize brands ####
brands <- lapply(
  files_list, function(x) {
    file <- paste0("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/", x)
    df <- readRDS(file)
    df$brand
  }
)

brands_df <- data.frame(brand = unlist(brands))

brands_count <- brands_df %>% 
  group_by(brand) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(
    total = sum(n),
    perc = n / total,
    cumsum_perc = cumsum(perc),
    brand = if_else(
      cumsum_perc <= 0.9, brand, "Other"
    )
  )

brands_map <- brands_count %>% 
  select(brand) %>% 
  filter(!duplicated(brand)) %>% 
  mutate(
    brand_group = if_else(brand != "Other", "major", "minor")
  )


#### Analyze station_type ####

#### Categorize brands ####
station_types <- lapply(
  files_list, function(x) {
    file <- paste0("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/", x)
    df <- readRDS(file)
    df$station_type
  }
)

station_type_df <- data.frame(station_type = unlist(station_types))

station_type_count <- station_type_df  %>% 
  group_by(station_type) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(
    station_type = replace_na(station_type, "Altro")
  )  %>% 
  group_by(station_type) %>% 
  summarize(
    n = sum(n)
  ) %>% 
  ungroup() %>% 
  arrange(desc(n))




