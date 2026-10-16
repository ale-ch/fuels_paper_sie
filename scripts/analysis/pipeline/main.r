library(foreach)
library(doParallel)
library(dplyr)
library(plm)
library(splm)
library(spdep)
library(sf)
library(tidyverse)

# Source logger FIRST
source("/Volumes/T7 Shield/FRES/repos/fuels_paper_sie/scripts/utils/logger.r")

# Source remaining scripts
source("/Volumes/T7 Shield/FRES/repos/fuels_paper_sie/scripts/utils/merge_shp_nuts.r")
source("/Volumes/T7 Shield/FRES/repos/fuels_paper_sie/scripts/analysis/pipeline/estimation.r")
source("/Volumes/T7 Shield/FRES/repos/fuels_paper_sie/scripts/analysis/pipeline/analyze_results.r")
source("/Volumes/T7 Shield/FRES/repos/fuels_paper_sie/scripts/analysis/pipeline/setup_pipeline.r")

# ------------------------------------------------------------------------------
# MAIN
# ------------------------------------------------------------------------------
main <- function(data_dir, output_dir, formula, radiuses, window_size, estimation_granularity, sample_fraction=1.0, random_seed, test=FALSE) {
  
  setup <- setup_pipeline(data_dir, output_dir, estimation_granularity, random_seed, test)
  
  run_dir <- setup$run_dir
  base_data_path <- setup$base_data_path 
  geo_data_path <- setup$geo_data_path
  results_file_path <- setup$results_file_path
  
  # Initialize centralized logging for the entire run
  setup_logging(run_dir, config)
  
  log_msg("Starting estimation phase...")
  run_rolling_estimation(base_data_path, config, formula, radiuses, window_size, 
                         estimation_granularity, run_dir,
                         sample_fraction, random_seed)
  
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