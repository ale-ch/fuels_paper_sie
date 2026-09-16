library(tidyverse)

list.files("/Volumes/T7 Shield/FRES/fuels_data/output_rdata")

files <- list.files("/Volumes/T7 Shield/FRES/fuels_data/output_rdata")


for(file in files) {
  input_file <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata", file)
  output_file <- file.path("/Volumes/T7 Shield/FRES/fuels_data/output_rdata", paste0(str_remove(file, ".csv"), ".RDS"))
  
  print(output_file)
  
  df <- read_delim(input_file, delim = "|")
  saveRDS(df, output_file)
}

