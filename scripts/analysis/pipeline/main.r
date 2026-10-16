#!/usr/bin/env Rscript

library(optparse)

option_list = list(
  make_option(c("--repo_dir"), type="character", default=".", help="Path to repository root"),
  make_option(c("--data_dir"), type="character", default=NULL, help="Path to data directory"),
  make_option(c("--output_dir"), type="character", default=NULL, help="Path to output directory"),
  make_option(c("--formula"), type="character", default="mean_price_gasoline_self ~ brent_diff_pos + brent_diff_neg", help="Model formula as a string"),
  make_option(c("--radiuses"), type="character", default="2500,5000", help="Comma-separated radiuses"),
  make_option(c("--window_size"), type="integer", default=52, help="Window size"),
  make_option(c("--estimation_granularity"), type="character", default="nuts1", help="Estimation granularity"),
  make_option(c("--random_seed"), type="integer", default=42, help="Random seed"),
  make_option(c("--sample_fraction"), type="double", default=0.05, help="Sample fraction"),
  make_option(c("--test"), action="store_true", default=FALSE, help="Boolean flag for test mode"),
  make_option(c("--verbose"), type="logical", default=TRUE, help="Print logs to console (TRUE/FALSE)"),
  make_option(c("--run_dir"),  type="character", default=NULL, help="Path to existing run directory"),
  make_option(c("--cores"), type="integer", default=NULL,
              help="Parallel workers (default: detectCores()-1)")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if (is.null(opt$data_dir) || is.null(opt$output_dir)) {
  print_help(opt_parser)
  stop("Arguments --data_dir and --output_dir are required.", call.=FALSE)
}

# Convert string inputs to proper R objects
model_formula <- as.formula(opt$formula)
model_radiuses <- as.numeric(unlist(strsplit(opt$radiuses, ",")))

setwd(opt$repo_dir)

suppressPackageStartupMessages({
  library(foreach)
  library(doParallel)
  library(dplyr)
  library(plm)
  library(splm)
  library(spdep)
  library(sf)
  library(tidyverse)
})

source("scripts/utils/logger.r")
source("scripts/utils/merge_shp_nuts.r")
source("scripts/analysis/pipeline/estimation.r")
source("scripts/analysis/pipeline/analyze_results.r")
source("scripts/analysis/pipeline/setup_pipeline.r")
# ------------------------------------------------------------------------------
# MAIN
# ------------------------------------------------------------------------------
main <- function(
    data_dir, output_dir, formula, radiuses, 
    window_size, estimation_granularity, 
    sample_fraction=1.0, random_seed, test=FALSE, 
    verbose, run_dir, cores) {
  
  setup <- setup_pipeline(data_dir, output_dir, estimation_granularity, random_seed, test, run_dir)
  
  run_dir <- setup$run_dir
  base_data_path <- setup$base_data_path 
  geo_data_path <- setup$geo_data_path
  results_file_path <- setup$results_file_path
  
  # Initialize centralized logging for the entire run
  setup_logging(output_dir = output_dir, verbose = verbose)
  
  log_msg("Starting estimation phase...")
  run_rolling_estimation(base_data_path, config, formula, radiuses, window_size, 
                         estimation_granularity, run_dir,
                         sample_fraction, random_seed,
                         cores = opt$cores)
  
  log_msg("Collecting rolling window results...")
  results <- collect_results(run_dir, results_file_path)
  
  log_msg("Calculating summary statistics...")
  summary_stats <- generate_summary_table(results)
  
  log_msg("Exporting presentation tables...")
  export_compact_tables(summary_stats, run_dir)
  
  log_msg("Initiating batch visualization pipeline...")
  generate_all_visualizations(
    stats_data = results, 
    summary_stats = summary_stats, 
    output_dir = run_dir, 
    geo_data_path = geo_data_path, # Passed to internal functions requiring shp_path
    estimation_granularity = estimation_granularity
  )
  
  log_msg("Pipeline completely finished.")
}



main(
  data_dir = opt$data_dir,
  output_dir = opt$output_dir,
  formula = model_formula,
  radiuses = model_radiuses,
  window_size = opt$window_size,
  estimation_granularity = opt$estimation_granularity,
  sample_fraction = opt$sample_fraction,
  random_seed = opt$random_seed,
  test = opt$test,
  verbose = opt$verbose,
  run_dir = opt$run_dir,
  cores = opt$cores
)