# ------------------------------------------------------------------------------
# 4. DIAGNOSTICS & COMPARISONS (UPDATED FOR NEW BATCH RESULTS STRUCTURE)
# ------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggplot2)
library(jsonlite)

# ------------------------------------------------------------------------------
# UPDATED VISUALIZATION & TABLE FUNCTIONS
# ------------------------------------------------------------------------------

plot_comparative_timeseries <- function(coef_df, target_term, title, is_split = TRUE) {
  plot_data <- coef_df %>% filter(term == target_term)
  if (nrow(plot_data) == 0) return(NULL)
  
  plot_data <- plot_data %>% 
    mutate(significance = ifelse(p_value < 0.05, "p < 0.05", "p >= 0.05"))
  
  p <- ggplot(plot_data, aes(x = end_date, y = estimate, color = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line(linewidth = 0.8, alpha = 0.6) +
    geom_point(aes(shape = significance), size = 2) +
    scale_shape_manual(values = c("p < 0.05" = 19, "p >= 0.05" = 4)) +
    theme_minimal() +
    labs(title = title, x = "Rolling Window End Date", y = "Estimate", color = "Model", shape = "Significance")
  
  if (is_split && length(unique(plot_data$region)) > 1) {
    p <- p + facet_wrap(~ region, scales = "free_y")
  }
  return(p)
}

generate_summary_table <- function(stats_data) {
  summary_table <- stats_data %>%
    group_by(region, term, model) %>%
    summarize(
      mean_estimate = mean(estimate, na.rm = TRUE),
      median_estimate = median(estimate, na.rm = TRUE),
      sd_estimate = sd(estimate, na.rm = TRUE),
      mean_p_value = mean(p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(region, term, model)
  
  return(summary_table)
}

plot_summary_statistics <- function(summary_table, terms_to_plot = NULL) {
  plot_data <- summary_table %>% filter(term != "(Intercept)")
  if (!is.null(terms_to_plot)) plot_data <- plot_data %>% filter(term %in% terms_to_plot)
  if (nrow(plot_data) == 0) return(NULL)
  
  plot_data <- plot_data %>% 
    mutate(is_sig = ifelse(mean_p_value < 0.05, "Sig (p<0.05)", "Not Sig"))
  
  p <- ggplot(plot_data, aes(y = reorder(model, mean_estimate), x = mean_estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_linerange(aes(xmin = mean_estimate - sd_estimate, xmax = mean_estimate + sd_estimate, alpha = is_sig), linewidth = 1.2) +
    geom_point(aes(alpha = is_sig), size = 3.5) +
    geom_point(aes(x = median_estimate), size = 1.5, shape = 21, fill = "white", stroke = 1) +
    scale_alpha_manual(values = c("Sig (p<0.05)" = 1, "Not Sig" = 0.3)) +
    theme_minimal() +
    labs(
      title = "Coefficient Stability and Central Tendency",
      subtitle = "Solid dot = Mean | White dot = Median | Lines = ±1 SD\nTransparent = Mean p-value >= 0.05",
      x = "Coefficient Estimate", y = "Model Specification", alpha = "Significance"
    ) +
    theme(legend.position = "right", strip.text = element_text(face = "bold", size = 11),
          axis.text.y = element_text(face = "bold", size = 10), panel.grid.minor = element_blank())
  
  if ("region" %in% colnames(plot_data) && length(unique(plot_data$region)) > 1) {
    p <- p + facet_grid(region ~ term, scales = "free_x")
  } else {
    p <- p + facet_wrap(~ term, scales = "free_x", ncol = 2)
  }
  return(p)
}

export_compact_tables <- function(summary_stats, output_dir) {
  tables_dir <- file.path(output_dir, "presentation_tables")
  if (!dir.exists(tables_dir)) dir.create(tables_dir, recursive = TRUE)
  
  format_sig <- function(est, pval) {
    stars <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
    paste0(round(est, 4), stars)
  }
  
  model_comp <- summary_stats %>%
    group_by(term, model) %>%
    summarize(
      est = mean(mean_estimate, na.rm = TRUE),
      pval = mean(mean_p_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(formatted_val = format_sig(est, pval)) %>%
    select(term, model, formatted_val) %>%
    pivot_wider(names_from = model, values_from = formatted_val)
  
  write_csv(model_comp, file.path(tables_dir, "table_1_model_comparison_with_stars.csv"))
  
  region_comp <- summary_stats %>%
    mutate(formatted_val = format_sig(mean_estimate, mean_p_value)) %>%
    select(region, term, model, formatted_val) %>%
    pivot_wider(names_from = region, values_from = formatted_val) %>%
    arrange(model, term)
  
  write_csv(region_comp, file.path(tables_dir, "table_2_regional_breakdown_with_stars.csv"))
  
  writeLines("Significance codes: 0 '***' 0.01 '**' 0.05 '*' 0.1", file.path(tables_dir, "legend.txt"))
  log_msg("Compact presentation tables with significance stars saved to: ", tables_dir)
}

# ------------------------------------------------------------------------------
# 6. SENSITIVITY VISUALIZATION
# ------------------------------------------------------------------------------
plot_radius_sensitivity <- function(all_radiuses_df, output_dir) {
  sens_dir <- file.path(output_dir, "sensitivity_plots")
  if (!dir.exists(sens_dir)) dir.create(sens_dir, recursive = TRUE)
  
  plot_data <- all_radiuses_df %>% filter(term != "(Intercept)")
  unique_terms <- unique(plot_data$term)
  
  log_msg("Generating sensitivity plots...")
  
  for (t in unique_terms) {
    
    if(length(unique(plot_data$region)) == 1) {
      sub_data <- plot_data %>% 
        filter(term == t) %>%
        group_by(model, term, radius_m)
    } else {
      sub_data <- plot_data %>% 
        filter(term == t) %>%
        group_by(region, model, term, radius_m)
    }
    
    sub_data <- sub_data %>% 
      reframe(
        region = if("region" %in% names(.)) region else NA,
        mean_estimate = mean(estimate, na.rm = TRUE),
        sd_estimate = sd(estimate, na.rm = TRUE),
        mean_p_value = mean(p_value, na.rm = TRUE)
      ) %>%
      mutate(is_sig = ifelse(mean_p_value < 0.05, "Sig (p<0.05)", "Not Sig"))
    
    if (nrow(sub_data) == 0) next
    
    p <- ggplot(sub_data, aes(x = as.factor(radius_m), y = mean_estimate, color = model, group = model)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
      geom_point(aes(shape = is_sig), size = 3, position = position_dodge(width = 0.3)) +
      geom_errorbar(aes(ymin = mean_estimate - sd_estimate, ymax = mean_estimate + sd_estimate), 
                    width = 0.2, position = position_dodge(width = 0.3), alpha = 0.5) +
      scale_shape_manual(values = c("Sig (p<0.05)" = 19, "Not Sig" = 4)) +
      theme_minimal() +
      labs(title = paste("Sensitivity to Spatial Radius:", t),
           x = "Radius (meters)", y = "Mean Coefficient Estimate", shape = "Significance") +
      theme(legend.position = "right", strip.text = element_text(face = "bold", size = 11))
    
    if ("region" %in% names(sub_data) && !all(is.na(sub_data$region)) && length(unique(sub_data$region)) > 1) {
      p <- p + facet_wrap(~ region, scales = "free_y")
    }
    
    safe_term <- gsub("[^A-Za-z0-9]", "_", t)
    ggsave(file.path(sens_dir, paste0("sensitivity_", safe_term, ".png")), plot = p, width = 10, height = 6, bg = "white")
  }
  
  log_msg("Sensitivity plots saved to: ", sens_dir)
}

plot_coefficient_maps <- function(summary_table, geo_data_path, 
                                  target_nuts_level = "nuts1", target_model = "SDM", 
                                  target_term = "Spatial Lag (rho)") {
  
  shp_data <- readRDS(geo_data_path)
  
  level_sym <- sym(target_nuts_level)
  
  region_polygons <- shp_data %>%
    group_by(!!level_sym) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    rename(region = !!level_sym) 
  
  stats_subset <- summary_table %>% filter(model == target_model, term == target_term)
  if (nrow(stats_subset) == 0) return(NULL)
  
  map_data <- region_polygons %>% left_join(stats_subset, by = "region")
  
  p_map <- ggplot(data = map_data) +
    geom_sf(aes(fill = mean_estimate), color = "white", linewidth = 0.3) +
    scale_fill_viridis_c(option = "magma", na.value = "grey80") +
    theme_void() +
    labs(
      title = sprintf("Geographic Distribution: %s", target_term),
      subtitle = sprintf("Model: %s | Aggregation: %s", target_model, toupper(target_nuts_level)),
      fill = "Mean\nEstimate"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right"
    )
  
  return(list(plot = p_map, map_data = map_data))
}

# ------------------------------------------------------------------------------
# 5. BATCH VISUALIZATION GENERATOR
# ------------------------------------------------------------------------------

generate_all_visualizations <- function(stats_data, summary_stats, output_dir, geo_data_path, estimation_granularity) {
  
  dirs <- list(
    timeseries = file.path(output_dir, "plots", "timeseries"),
    maps       = file.path(output_dir, "plots", "maps"),
    summaries  = file.path(output_dir, "plots", "summaries")
  )
  lapply(dirs, function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE))
  
  unique_terms <- unique(stats_data$term)
  unique_models <- unique(stats_data$model)
  
  log_msg("Generating all plots. This may take a moment...")
  
  for (t in unique_terms) {
    if (t == "(Intercept)") next
    
    safe_term <- gsub("[^A-Za-z0-9]", "_", t)
    
    p_ts <- plot_comparative_timeseries(stats_data, t, paste("Time Series:", t), is_split = TRUE)
    if (!is.null(p_ts)) {
      ggsave(file.path(dirs$timeseries, paste0("ts_", safe_term, ".png")), plot = p_ts, width = 12, height = 8, bg = "white")
    }
    
    p_sum <- plot_summary_statistics(summary_stats, terms_to_plot = t)
    if (!is.null(p_sum)) {
      ggsave(file.path(dirs$summaries, paste0("summary_", safe_term, ".png")), plot = p_sum, width = 10, height = 8, bg = "white")
    }
    
    for (m in unique_models) {
      safe_model <- gsub("[^A-Za-z0-9]", "_", m)
      
      tryCatch({
        map_out <- plot_coefficient_maps(summary_stats, geo_data_path, 
                                         target_nuts_level = estimation_granularity, 
                                         target_model = m, target_term = t)
        
        if (!is.null(map_out$plot)) {
          ggsave(file.path(dirs$maps, paste0("map_", safe_model, "_", safe_term, "_", estimation_granularity, ".png")), 
                 plot = map_out$plot, width = 10, height = 8, bg = "white")
        }
      }, error = function(e) {
      })
    }
  }
  
  p_all_sum <- plot_summary_statistics(summary_stats, terms_to_plot = NULL)
  if (!is.null(p_all_sum)) {
    ggsave(file.path(output_dir, "plots", "summary_ALL_TERMS.png"), plot = p_all_sum, width = 16, height = 12, bg = "white")
  }
  
  plot_radius_sensitivity(stats_data, output_dir)
  
  log_msg("All plots successfully generated and organized in: ", file.path(output_dir, "plots"))
}
