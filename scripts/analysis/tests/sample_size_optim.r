library(foreach)
library(doParallel)
library(dplyr)
library(plm)
library(splm)
library(spdep)
library(sf)
library(ggplot2)
library(tidyr)

evaluate_sample_stability_spgm <- function(base_data, config, sample_sizes = c(20, 30, 40, 50, 75, 100), 
                                           radius_m = 10000, iterations = 5, 
                                           start_time = NULL, end_time = NULL, 
                                           spatial_opts = list(lag = TRUE, spatial.error = TRUE, Durbin = FALSE)) {
  
  df <- base_data$df
  stations_sf <- base_data$stations_sf
  
  # Apply time range filtering
  if (!is.null(start_time)) {
    df <- df %>% filter(time_index >= start_time)
  }
  if (!is.null(end_time)) {
    df <- df %>% filter(time_index <= end_time)
  }
  
  if (nrow(df) == 0) {
    stop("No data available in the selected time range.")
  }
  
  time_periods <- sort(unique(df$time_index))
  required_periods <- length(time_periods)
  
  cat(sprintf("Time range selected: %s to %s (%d periods)\n", min(time_periods), max(time_periods), required_periods))
  
  all_vars <- c(config$dependent_var, config$base_features, config$conditional_features)
  
  # 1. Ensure strictly balanced panel within the selected time range
  complete_pumps <- df %>%
    select(id_pump, time_index, nuts3, all_of(all_vars)) %>%
    filter(complete.cases(.)) %>%
    count(id_pump) %>% 
    filter(n == required_periods) %>% 
    pull(id_pump)
  
  df_balanced <- df %>% filter(id_pump %in% complete_pumps)
  
  if (nrow(df_balanced) == 0) {
    stop("No stations have a strictly balanced panel in the selected time range.")
  }
  
  # 2. Filter viable regions
  pump_counts <- df_balanced %>%
    select(nuts3, id_pump) %>%
    distinct() %>%
    count(nuts3, name = "total_pumps") %>%
    filter(total_pumps >= min(sample_sizes))
  
  valid_regions <- pump_counts$nuts3
  df_filtered <- df_balanced %>% filter(nuts3 %in% valid_regions)
  
  base_vars <- paste(config$base_features, collapse = " + ")
  cond_vars <- paste(config$conditional_features, collapse = " + ")
  form <- as.formula(paste(config$dependent_var, "~", base_vars, "+", cond_vars))
  
  results <- list()
  timings <- list()
  
  cat("Evaluating spgm stability across stratified sample sizes...\n")
  
  for (size in sample_sizes) {
    cat(sprintf("\n--- Testing max sample size per strata: %d ---\n", size))
    
    for (i in 1:iterations) {
      cat(sprintf("  Iteration %d/%d...\n", i, iterations))
      
      # Step A: Stratified sampling
      cat("    -> Sampling data...")
      t0_samp <- Sys.time()
      sampled_ids <- df_filtered %>%
        select(nuts3, id_pump) %>%
        distinct() %>%
        group_by(nuts3) %>%
        mutate(
          count = n(),
          slice_size = min(count, size)
        ) 
      
      slice_size <- min(sampled_ids$count, size)
      sampled_ids <- sampled_ids %>% 
        slice_sample(n = slice_size) %>%
        ungroup() %>%
        pull(id_pump)
      
      df_sample <- df_filtered %>% 
        filter(id_pump %in% sampled_ids) %>% 
        arrange(id_pump, time_index)
      
      st_sample <- stations_sf %>% 
        filter(id_pump %in% sampled_ids) %>% 
        arrange(id_pump) %>% 
        st_as_sf()
      t1_samp <- Sys.time()
      cat(" Done.\n")
      
      # Step B: Compute Spatial Weights (W)
      cat("    -> Computing spatial weights...")
      t0_weights <- Sys.time()
      coords <- st_coordinates(st_sample)[, 1:2, drop = FALSE]
      nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
      
      isolated <- which(card(nb) == 0)
      if (length(isolated) > 0) {
        st_sample <- st_sample[-isolated, ]
        df_sample <- df_sample %>% filter(id_pump %in% st_sample$id_pump)
        coords <- st_coordinates(st_sample)[, 1:2, drop = FALSE]
        nb <- dnearneigh(coords, d1 = 0, d2 = radius_m)
      }
      
      if (nrow(st_sample) < 30) {
        cat(" Skipped (Not enough points after removing isolated stations).\n")
        next
      }
      
      W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x > 0, 1 / x, 0)), 
                    style = "W", zero.policy = TRUE)
      
      p_data <- pdata.frame(df_sample, index = c("id_pump", "time_index"))
      t1_weights <- Sys.time()
      cat(" Done.\n")
      
      # Step C: Estimate Model
      cat("    -> Estimating SPGM model...")
      t0_est <- Sys.time()
      fit <- tryCatch({
        spgm(
          formula = form,
          data = p_data,
          listw = W,
          lag = spatial_opts$lag,
          spatial.error = spatial_opts$spatial.error,
          Durbin = spatial_opts$Durbin,
          method = "w2sls"
        )
      }, error = function(e) {
        cat(sprintf(" Error: %s ", e$message))
        NULL
      })
      t1_est <- Sys.time()
      
      if(!is.null(fit)) cat(" Done.\n") else cat("\n")
      
      # Log timings
      time_samp <- as.numeric(difftime(t1_samp, t0_samp, units = "secs"))
      time_weights <- as.numeric(difftime(t1_weights, t0_weights, units = "secs"))
      time_est <- as.numeric(difftime(t1_est, t0_est, units = "secs"))
      time_total <- time_samp + time_weights + time_est
      
      timings[[length(timings) + 1]] <- data.frame(
        sample_size_strata = size,
        iteration = i,
        time_sampling_sec = time_samp,
        time_weights_sec = time_weights,
        time_estimation_sec = time_est,
        time_total_sec = time_total
      )
      
      if (!is.null(fit)) {
        res_summary <- summary(fit)
        coef_matrix <- res_summary$CoefTable
        
        results[[length(results) + 1]] <- data.frame(
          sample_size_strata = size,
          iteration = i,
          variable = rownames(coef_matrix),
          estimate = coef_matrix[, 1],
          std_error = coef_matrix[, 2],
          total_n = nrow(st_sample),
          row.names = NULL
        )
      }
    }
    
    # Print average timing for current sample size
    current_timings <- bind_rows(timings) %>% filter(sample_size_strata == size)
    if(nrow(current_timings) > 0) {
      cat(sprintf("Average Times (secs) - Sampling: %.2f | Weights: %.2f | Estimation: %.2f | Total: %.2f\n",
                  mean(current_timings$time_sampling_sec),
                  mean(current_timings$time_weights_sec),
                  mean(current_timings$time_estimation_sec),
                  mean(current_timings$time_total_sec)))
    } else {
      cat("No successful iterations for this sample size to calculate averages.\n")
    }
  }
  
  final_results <- bind_rows(results)
  timing_results <- bind_rows(timings)
  
  stability_metrics <- final_results %>%
    group_by(sample_size_strata, variable) %>%
    summarise(
      mean_estimate = mean(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      mean_se = mean(std_error, na.rm = TRUE),
      valid_iterations = n(),
      .groups = "drop"
    ) %>%
    arrange(variable, sample_size_strata)
  
  return(list(raw = final_results, summary = stability_metrics, timings = timing_results))
}

plot_spgm_stability <- function(stability_data) {
  ggplot(stability_data$summary, aes(x = sample_size_strata, y = mean_se, color = variable)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ variable, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "SPGM Coefficient Standard Error vs. Sample Size per Strata",
      subtitle = "Visualizing diminishing returns in standard error reduction",
      x = "Target Sample Size per Strata (nuts3)",
      y = "Mean Standard Error"
    ) +
    theme(legend.position = "none")
}

# Example usage:
config <- list(
    dependent_var = "mean_price_gasoline_self",
    base_features = c("brent_diff_pos", "brent_diff_neg"),
    conditional_features = c("population_density")
  )
# 
results <- evaluate_sample_stability_spgm(base_data, config, 
                                             sample_sizes = c(20, 40, 60), 
                                             radius_m = 10000, 
                                             iterations = 10,
                                             start_time = "2023-01",
                                             end_time = "2023-03")
plot_spgm_stability(results)