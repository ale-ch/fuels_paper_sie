# ------------------------------------------------------------------------------
# 4. DIAGNOSTICS & COMPARISONS (UPDATED)
# ------------------------------------------------------------------------------
analyze_comparative_diagnostics <- function(rolling_output) {
  rolling_results <- rolling_output$results
  is_split <- rolling_output$split_by_nuts1 
  
  coef_df <- map_dfr(names(rolling_results), function(region_name) {
    region_data <- rolling_results[[region_name]]
    map_dfr(names(region_data), function(win) {
      mod_data <- region_data[[win]]
      map_dfr(names(mod_data$models), function(m_name) {
        summ <- summary(mod_data$models[[m_name]])
        c_df <- as.data.frame(summ$CoefTable)
        colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
        c_df$term <- rownames(c_df)
        c_df$model <- m_name
        c_df$window <- win
        c_df$end_week <- mod_data$end_week
        c_df$nuts1 <- region_name
        c_df$date <- as.Date(paste0(mod_data$end_week, "-1"), format = "%Y-%W-%u")
        return(c_df)
      })
    })
  })
  rownames(coef_df) <- NULL
  
  # Use grepl for robust matching ignoring spaces
  coef_df <- coef_df %>% mutate(term = case_when(
    grepl("lambda|rho", term, ignore.case = TRUE) ~ "Spatial Lag (rho)",
    grepl("mean_price_gasoline_self", term) ~ "Time Lag (Gasoline)",
    grepl("brent_price_mean", term) ~ "Time Lag (Brent)",
    grepl("brent_diff_pos", term) ~ "Time Lag (Brent Δ+)",
    grepl("brent_diff_neg", term) ~ "Time Lag (Brent Δ-)",
    TRUE ~ term
  ))
  
  plot_comparative <- function(target_term, title) {
    plot_data <- coef_df %>% filter(term == target_term)
    
    # Safeguard against missing terms
    if (nrow(plot_data) == 0) {
      warning(paste("No data found for term:", target_term, "- Check model output names."))
      return(ggplot() + theme_void() + labs(title = paste("Missing data for", target_term)))
    }
    
    p <- ggplot(plot_data, aes(x = date, y = estimate, color = model)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_line(linewidth = 0.8) +
      theme_minimal() +
      labs(title = title, x = "Date", y = "Estimate", color = "Model")
    
    if (is_split) {
      p <- p + facet_wrap(~ nuts1, scales = "free_y") +
        labs(subtitle = "Faceted by nuts1 Region")
    }
    return(p)
  }
  
  p1 <- plot_comparative("Spatial Lag (rho)", "Comparison of Spatial Lag Estimates")
  p2 <- plot_comparative("Time Lag (Brent Δ+)", "Comparison of Positive Brent Price Change Transmission")
  
  print(p1)
  print(p2)
  
  return(list(stats_data = coef_df, plots = list(p1, p2)))
}



analyze_comparative_diagnostics <- function(rolling_output) {
  is_split <- rolling_output$split_by_nuts1 
  file_paths <- rolling_output$file_paths
  
  coef_df <- map_dfr(file_paths, function(file_path) {
    mod_data <- readRDS(file_path)
    
    c_df_part <- map_dfr(names(mod_data$models), function(m_name) {
      summ <- summary(mod_data$models[[m_name]])
      c_df <- as.data.frame(summ$CoefTable)
      colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
      c_df$term <- rownames(c_df)
      c_df$model <- m_name
      c_df$window <- mod_data$window
      c_df$end_week <- mod_data$end_week
      c_df$nuts0 <- mod_data$region
      c_df$date <- as.Date(paste0(mod_data$end_week, "-1"), format = "%Y-%W-%u")
      return(c_df)
    })
    
    rm(mod_data)
    gc()
    
    return(c_df_part)
  })
  
  rownames(coef_df) <- NULL
  
  coef_df <- coef_df %>% mutate(term = case_when(
    grepl("lambda|rho", term, ignore.case = TRUE) ~ "Spatial Lag (rho)",
    grepl("mean_price_gasoline_self", term) ~ "Time Lag (Gasoline)",
    grepl("brent_price_mean", term) ~ "Time Lag (Brent)",
    grepl("brent_diff_pos", term) ~ "Time Lag (Brent Δ+)",
    grepl("brent_diff_neg", term) ~ "Time Lag (Brent Δ-)",
    TRUE ~ term
  ))
  
  plot_comparative <- function(target_term, title) {
    plot_data <- coef_df %>% filter(term == target_term)
    if (nrow(plot_data) == 0) return(ggplot() + theme_void() + labs(title = paste("Missing data for", target_term)))
    
    p <- ggplot(plot_data, aes(x = date, y = estimate, color = model)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_line(linewidth = 0.8) +
      theme_minimal() +
      labs(title = title, x = "Date", y = "Estimate", color = "Model")
    
    if (is_split) p <- p + facet_wrap(~ nuts0, scales = "free_y")
    return(p)
  }
  
  p1 <- plot_comparative("Spatial Lag (rho)", "Comparison of Spatial Lag Estimates")
  p2 <- plot_comparative("Time Lag (Brent Δ+)", "Comparison of Positive Brent Price Change")
  
  return(list(stats_data = coef_df, plots = list(p1, p2)))
}

# ------------------------------------------------------------------------------
# 5. MAP GENERATION FUNCTIONS
# ------------------------------------------------------------------------------

# 5A. Update Summary Function to Group by Region
generate_summary_table <- function(stats_data) {
  require(dplyr)
  
  summary_table <- stats_data %>%
    # Group by nuts1 alongside term and model to retain spatial granularity
    group_by(nuts1, term, model) %>%
    summarize(
      mean_estimate = mean(estimate, na.rm = TRUE),
      median_estimate = median(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      mean_p_value = mean(p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(nuts1, term, model)
  
  return(summary_table)
}

# 5B. Map Plotting Function
plot_coefficient_maps <- function(summary_table, shp_path, nuts_path, 
                                  target_model = "SDM", 
                                  target_term = "Spatial Lag (rho)") {
  require(sf)
  require(dplyr)
  require(ggplot2)
  require(jsonlite)
  
  # 1. Load and prepare geographic bounds
  nuts_data <- as.data.frame(fromJSON(nuts_path)$resultset)
  
  nuts_data <- nuts_data %>% 
    select(PRO_COM, contains("DEN")) %>% 
    rename(
      nuts1 = DEN_RIP,
      nuts2 = DEN_REG,
      nuts3 = DEN_UTS
    )
  
  # Ensure the join uses the correct municipal ID column matching your JSON and SHP
  shp_data <- st_read(shp_path, quiet = TRUE) %>%
    left_join(nuts_data, by = c("PRO_COM" = "PRO_COM"))
  
  # 2. Aggregate municipal geometries into nuts1 regional boundaries
  # Note: Adjust 'nuts1' here if your JSON mapping script named it differently
  region_polygons <- shp_data %>%
    group_by(nuts1) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  # 3. Filter statistical data for the requested map layer
  stats_subset <- summary_table %>%
    filter(model == target_model, term == target_term)
  
  # 4. Join geometries with the model estimates
  map_data <- region_polygons %>%
    left_join(stats_subset, by = "nuts1")
  
  # 5. Generate Choropleth Map
  p_map <- ggplot(data = map_data) +
    geom_sf(aes(fill = mean_estimate), color = "white", linewidth = 0.3) +
    scale_fill_viridis_c(option = "magma", na.value = "grey80") +
    theme_void() +
    labs(
      title = sprintf("Geographic Distribution: %s", target_term),
      subtitle = sprintf("Model Specification: %s | Aggregation: nuts1", target_model),
      fill = "Mean\nEstimate"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right"
    )
  
  print(p_map)
  return(list(plot = p_map, map_data = map_data))
}

# ------------------------------------------------------------------------------
# 6. PIPELINE WRAPPER AND EXECUTION SCRIPT
# ------------------------------------------------------------------------------

# 6A. Update plot_summary_statistics to handle nuts1 facets
plot_summary_statistics <- function(summary_table) {
  require(ggplot2)
  require(dplyr)
  
  plot_data <- summary_table %>% filter(term != "(Intercept)")
  
  p <- ggplot(plot_data, aes(y = reorder(model, mean_estimate), x = mean_estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_linerange(aes(xmin = mean_estimate - sd_estimate, 
                       xmax = mean_estimate + sd_estimate), 
                   linewidth = 1.2, alpha = 0.6) +
    geom_point(size = 3.5) +
    geom_point(aes(x = median_estimate), size = 1.5, shape = 21, fill = "white", stroke = 1) +
    theme_minimal() +
    labs(
      title = "Coefficient Stability and Central Tendency",
      subtitle = "Solid dot = Mean | White dot = Median | Lines = ±1 SD",
      x = "Coefficient Estimate",
      y = "Model Specification"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11),
      axis.text.y = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank()
    )
  
  # Facet dynamically depending on whether nuts1 is present
  if ("nuts1" %in% colnames(plot_data) && length(unique(plot_data$nuts1)) > 1) {
    p <- p + facet_grid(nuts1 ~ term, scales = "free_x")
  } else {
    p <- p + facet_wrap(~ term, scales = "free_x", ncol = 2)
  }
  
  print(p)
  return(p)
}
