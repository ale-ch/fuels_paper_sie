# ------------------------------------------------------------------------------
# 1. FUNCTION: RUN YEARLY MODELS
# ------------------------------------------------------------------------------
run_yearly_models <- function(years, data_file, stations_list_file, k = 10) {
  models_list <- list()
  
  for (yr in years) {
    cat(sprintf("Processing year: %d\n", yr))
    
    tryCatch({
      # Prepare data for the specific year
      prep <- prepare_spatial_panel(
        data_file = data_file, 
        stations_list = stations_list_file, 
        k = k, 
        target_year = yr
      )
      
      # Model formulation:
      # lag = TRUE incorporates the spatial lag of the dependent variable
      # lag(..., 1) incorporates the temporal lags
      # Durbin = FALSE ensures independent variables are not spatially lagged
      model <- spgm(
        mean_price_gasoline_self ~ lag(mean_price_gasoline_self, 1) + lag(brent_price_mean, 1),
        data = prep$df, 
        listw = prep$W, 
        lag = TRUE, 
        Durbin = FALSE, 
        spatial.error = FALSE, 
        method = "w2sls"
      )
      
      models_list[[as.character(yr)]] <- model
      cat(sprintf("Successfully estimated model for %d\n", yr))
      
    }, error = function(e) {
      message(sprintf("Failed for year %d: %s", yr, e$message))
    })
  }
  
  return(models_list)
}

# ------------------------------------------------------------------------------
# 2. FUNCTION: ANALYZE AND REPORT RESULTS
# ------------------------------------------------------------------------------
analyze_yearly_results <- function(models_list) {
  require(ggplot2)
  require(dplyr)
  require(knitr)
  
  # Extract coefficients into a consolidated dataframe
  coef_df <- do.call(rbind, lapply(names(models_list), function(yr) {
    mod <- models_list[[yr]]
    summ <- summary(mod)
    
    # Extract coefficients matrix
    coefs <- as.data.frame(summ$CoefTable)
    coefs$term <- rownames(coefs)
    coefs$year <- as.integer(yr)
    return(coefs)
  }))
  
  rownames(coef_df) <- NULL
  colnames(coef_df) <- c("estimate", "std.error", "t.value", "p.value", "term", "year")
  
  # Clean terminology for readability
  coef_df <- coef_df %>%
    mutate(term = case_when(
      term == "lambda" ~ "Spatial Lag (rho)",
      term == "lag(mean_price_gasoline_self, 1)" ~ "Time Lag (Gasoline t-1)",
      term == "lag(brent_price_mean, 1)" ~ "Time Lag (Brent t-1)",
      TRUE ~ term
    ))
  
  # Print formatted table to console
  cat("\n======================================================\n")
  cat("YEARLY COEFFICIENTS SUMMARY (2016-2026)\n")
  cat("======================================================\n\n")
  print(kable(coef_df %>% arrange(term, year), digits = 4))
  
  # Generate diagnostic plots
  plot_data <- coef_df %>% filter(term != "(Intercept)")
  
  p <- ggplot(plot_data, aes(x = year, y = estimate, color = term)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                      ymax = estimate + 1.96 * std.error), 
                  width = 0.2) +
    facet_wrap(~ term, scales = "free_y", ncol = 1) +
    scale_x_continuous(breaks = min(plot_data$year):max(plot_data$year)) +
    theme_minimal() +
    labs(
      title = "Evolution of Retail Gasoline Price Dynamics in Italy",
      subtitle = "Spatial Dependence, Price Persistence, and Brent Crude Transmission (95% CI)",
      x = "Year",
      y = "Coefficient Estimate"
    ) +
    theme(legend.position = "none",
          strip.text = element_text(face = "bold", size = 12))
  
  print(p)
  
  return(list(table = coef_df, plot = p))
}

# ------------------------------------------------------------------------------
# 3. EXECUTION SCRIPT
# ------------------------------------------------------------------------------
require(tidyverse)
require(sf)
require(spdep)
require(FNN)
require(plm)
require(splm)
require(quantmod)
require(zoo)
require(lubridate)

source("prepare_spatial_panel.r")

file_path <- "/Volumes/T7 Shield/FRES/fuels_data/output_rdata/weekly/part_0.RDS"
stations_list_file <- "/Volumes/T7 Shield/FRES/fuels_data/stations_data/stations_list.csv"

# Define years to loop through
target_years <- 2016:2026

# Run models
yearly_models <- run_yearly_models(
  years = target_years, 
  data_file = file_path, 
  stations_list_file = stations_list_file, 
  k = 10
)

# Analyze and plot results
analysis_output <- analyze_yearly_results(yearly_models)