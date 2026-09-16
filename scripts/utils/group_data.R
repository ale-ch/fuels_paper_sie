library(tidyverse)

# df <- readRDS(file)

group_by_week <- function(df) {
  df %>% 
    group_by(
      id_pump, year = year(date), week = week(date), brand, station_type, city, province, latitude, longitude
    ) %>% 
    reframe(
      mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
      mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
      mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
      mean_price_diesel = mean(price_diesel, na.rm = TRUE)
    ) %>% 
    ungroup()
}

group_by_month <- function(df) {
  df %>% 
    group_by(
      id_pump, year = year(date), month = month(date), brand, station_type, city, province, latitude, longitude
    ) %>% 
    reframe(
      mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
      mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
      mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
      mean_price_diesel = mean(price_diesel, na.rm = TRUE)
    ) %>% 
    ungroup()
}

group_by_quarter <- function(df) {
  df %>% 
    group_by(
      id_pump, year = year(date), quarter = quarter(date), brand, station_type, city, province, latitude, longitude
    ) %>% 
    reframe(
      mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
      mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
      mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
      mean_price_diesel = mean(price_diesel, na.rm = TRUE)
    ) %>% 
    ungroup()
}

#grouped_week_df <- group_by_week(df)
#grouped_month_df <- group_by_month(df)
#grouped_quarter_df <- group_by_quarter(df)


files <- list.files("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/daily")


for(file in files) {
  input_file <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/daily", file)
  
  print(input_file)
  
  df <- readRDS(input_file)
  
  output_file_week <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly", paste0(str_replace(str_remove(file, ".csv"), "chunk", "part")))
  output_file_month <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/monthly", paste0(str_replace(str_remove(file, ".csv"), "chunk", "part")))
  output_file_quarter <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/quarterly", paste0(str_replace(str_remove(file, ".csv"), "chunk", "part")))

  print(output_file_week)
  print(output_file_month)
  print(output_file_quarter)
  
  grouped_week_df <- group_by_week(df)
  saveRDS(grouped_week_df, output_file_week)
  
  grouped_month_df <- group_by_month(df)
  saveRDS(grouped_month_df, output_file_month)
  
  grouped_quarter_df <- group_by_quarter(df)
  saveRDS(grouped_quarter_df, output_file_quarter)
}





###### GROUP BY GAS STATION OR MUNICIPALITY 


library(tidyverse)


base_path <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/daily"

files <- list.files(base_path)

# df <- readRDS(file.path(base_path, files[1]))

group_by_week <- function(df, level = c("station", "municipal")) {
  if(level == "station") {
    df %>% 
      group_by(
        id_pump, year = year(date), week = week(date), brand, station_type, city, province, latitude, longitude
      ) %>% 
      reframe(
        mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
        mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
        mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
        mean_price_diesel = mean(price_diesel, na.rm = TRUE)
      ) %>% 
      ungroup()
  } else {
    df %>% 
      group_by(
        city, year = year(date), week = week(date), brand, station_type, city, province, latitude, longitude
      ) %>% 
      reframe(
        mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
        mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
        mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
        mean_price_diesel = mean(price_diesel, na.rm = TRUE)
      ) %>% 
      ungroup()
  }

}

group_by_month <- function(df, level = c("station", "municipal")) {
  if (level == "station") {
    df %>% 
      group_by(
        id_pump, year = year(date), month = month(date), brand, station_type, city, province, latitude, longitude
      ) %>% 
      reframe(
        mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
        mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
        mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
        mean_price_diesel = mean(price_diesel, na.rm = TRUE)
      ) %>% 
      ungroup()
  } else {
    df %>% 
      group_by(
        city, year = year(date), month = month(date), brand, station_type, city, province, latitude, longitude
      ) %>% 
      reframe(
        mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
        mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
        mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
        mean_price_diesel = mean(price_diesel, na.rm = TRUE)
      ) %>% 
      ungroup()
  }
  
}

group_by_quarter <- function(df, level = c("station", "municipal")) {
  if (level == "station") {
    df %>% 
      group_by(
        id_pump, year = year(date), quarter = quarter(date), brand, station_type, city, province, latitude, longitude
      ) %>% 
      reframe(
        mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
        mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
        mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
        mean_price_diesel = mean(price_diesel, na.rm = TRUE)
      ) %>% 
      ungroup()
  } else {
    df %>% 
      group_by(
        city, year = year(date), quarter = quarter(date), brand, station_type, city, province, latitude, longitude
      ) %>% 
      reframe(
        mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
        mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
        mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
        mean_price_diesel = mean(price_diesel, na.rm = TRUE)
      ) %>% 
      ungroup()
  }
}



municipal_daily <- df %>% 
  group_by(
    city, province, date #, city, brand, station_type, latitude, longitude
  ) %>% 
  reframe(
    mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
    mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
    mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
    mean_price_diesel = mean(price_diesel, na.rm = TRUE)
  ) %>% 
  ungroup()

municipal_daily <- df %>% 
  group_by(city, province, date) %>% 
  reframe(
    n_pumps = n_distinct(id_pump),
    mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
    mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
    mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
    mean_price_diesel = mean(price_diesel, na.rm = TRUE)
  ) %>% 
  ungroup()


df %>% 
  group_by(city) %>% 
  summarize(
    n = n_distinct(id_pump)
  ) %>% 
  arrange(desc(n))



#grouped_week_df <- group_by_week(df)
#grouped_month_df <- group_by_month(df)
#grouped_quarter_df <- group_by_quarter(df)


files <- list.files("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/daily/station")


for(file in files) {
  input_file <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/daily/station", file)
  
  print(input_file)
  
  df <- readRDS(input_file)
  
  output_file_day <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/daily/municipal", file)
  output_file_week <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/municipal", paste0(str_replace(str_remove(file, ".csv"), "chunk", "part")))
  output_file_month <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/monthly/municipal", paste0(str_replace(str_remove(file, ".csv"), "chunk", "part")))
  output_file_quarter <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata/quarterly/municipal", paste0(str_replace(str_remove(file, ".csv"), "chunk", "part")))
  
  print(output_file_day)
  print(output_file_week)
  print(output_file_month)
  print(output_file_quarter)
  
  municipal_daily <- df %>% 
    group_by(city, province, date) %>% 
    reframe(
      n_pumps = n_distinct(id_pump),
      mean_price_gasoline_self = mean(price_gasoline_self, na.rm = TRUE),
      mean_price_gasoline = mean(price_gasoline, na.rm = TRUE),
      mean_price_diesel_self = mean(price_diesel_self, na.rm = TRUE),
      mean_price_diesel = mean(price_diesel, na.rm = TRUE)
    ) %>% 
    ungroup()
  
  
  saveRDS(municipal_daily, output_file_day)
  
  grouped_week_df <- group_by_week(df, level = "municipal")
  saveRDS(grouped_week_df, output_file_week)
  
  grouped_month_df <- group_by_month(df, level = "municipal")
  saveRDS(grouped_month_df, output_file_month)
  
  grouped_quarter_df <- group_by_quarter(df, level = "municipal")
  saveRDS(grouped_quarter_df, output_file_quarter)
}


