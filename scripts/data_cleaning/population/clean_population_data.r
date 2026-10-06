library(tidyverse)
library(readxl)

# Define base paths
base_dir <- "/Volumes/T7 Shield/FRES/fuels_data/population"
singola_area_dir <- file.path(base_dir, "PopolazioneEta-SingolaArea-Comuni")

# ==========================================
# 1. Process POSAS files
# ==========================================
posas_files <- list.files(base_dir, pattern = "POSAS", full.names = TRUE)

population_posas <- map_dfr(posas_files, function(file_path) {
  extracted_year <- as.numeric(str_extract(basename(file_path), "\\d{4}"))
  
  read_delim(file_path, delim = ";", skip = 1, show_col_types = FALSE) %>% 
    rename(
      id_city = `Codice comune`,
      city = Comune, 
      population = Totale,
      age = Età
    ) %>% 
    filter(age == 999, extracted_year > 2019) %>% 
    mutate(year = extracted_year) %>% 
    select(id_city, city, year, population)
})

# ==========================================
# 2. Process Singola Area files
# ==========================================
process_singola_area <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)
  lines <- lines[lines != ""]
  
  # Remove quotes and extract metadata using stringr
  first_line <- str_remove_all(lines[1], '"')
  meta_match <- str_match(first_line, "Comune:\\s*(\\d+)\\s*-\\s*(.+)$")
  
  if (is.na(meta_match[1])) return(NULL)
  
  id_city <- meta_match[2]
  city <- meta_match[3]
  
  # Isolate header and the first "Totale;" row
  header_idx <- grep("Età/Anno", lines)[1]
  totale_idx <- grep("^\"?Totale;", lines)[1]
  
  if (is.na(header_idx) || is.na(totale_idx)) return(NULL)
  
  # Parse lines into a dataframe and reshape
  read.csv(text = paste(lines[header_idx], lines[totale_idx], sep = "\n"), 
           sep = ";", check.names = FALSE, stringsAsFactors = FALSE) %>%
    pivot_longer(cols = -1, names_to = "year", values_to = "population") %>%
    mutate(
      id_city = as.numeric(id_city),
      city = city,
      year = as.numeric(year),
      population = as.numeric(population)
    ) %>%
    select(id_city, city, year, population)
}

singola_area_files <- list.files(singola_area_dir, pattern = "\\.csv$", full.names = TRUE)
population_singola_area <- map_dfr(singola_area_files, process_singola_area)

# ==========================================
# 3. Combine Datasets
# ==========================================
pop_final <- bind_rows(
  mutate(population_posas, id_city = as.numeric(id_city)), 
  population_singola_area
) %>% 
  filter(year >= 2016) %>% 
  arrange(id_city, year)

print(pop_final)
