source("/Volumes/T7 Shield/FRES/repos/fuels_paper_sie/scripts/analysis/pipeline/main.r")

data_dir <- "/Volumes/T7 Shield/FRES/fuels_data/data"
output_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results"
formula <- mean_price_gasoline_self ~ brent_diff_pos + brent_diff_neg
radiuses <- c(2500, 5000)
window_size <- 52
estimation_granularity <- "nuts1"
random_seed <- 42
sample_fraction <- 0.05
test <- TRUE


main(data_dir, output_dir, formula, radiuses, window_size, estimation_granularity, sample_fraction, random_seed, test) 
