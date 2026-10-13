# ------------------------------------------------------------------------------
# 4. DIAGNOSTICS & COMPARISONS (UPDATED FOR BATCH PLOT GENERATION)
# ------------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggplot2)
library(jsonlite)

analyze_standalone_results <- function(parts_dir, config, is_split = TRUE) {
  file_paths <- list.files(parts_dir, pattern = "\\.RDS$", full.names = TRUE)
  if(length(file_paths) == 0) stop("No RDS files found in the specified directory.")
  
  coef_df <- map_dfr(file_paths, function(file_path) {
    mod_data <- readRDS(file_path)
    
    c_df_part <- map_dfr(names(mod_data$models), function(m_name) {
      
      # 1. Safely attempt to generate the summary
      summ <- tryCatch({
        summary(mod_data$models[[m_name]])
      }, error = function(e) {
        return(NULL) # Skip if the internal summary method crashes
      })
      
      if (is.null(summ)) return(NULL)
      
      # 2. Extract table safely
      if ("CoefTable" %in% names(summ)) {
        c_df <- as.data.frame(summ$CoefTable)
      } else if ("coefficients" %in% names(summ)) {
        c_df <- as.data.frame(summ$coefficients)
      } else {
        return(NULL)
      }
      
      # 3. Skip if the model failed and returned a degenerate table
      if (nrow(c_df) == 0 || ncol(c_df) < 4) {
        return(NULL)
      }
      
      # 4. Force standard dimensions and names
      c_df <- c_df[, 1:4]
      colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
      
      c_df$term <- rownames(c_df)
      c_df$model <- m_name
      c_df$window <- mod_data$window
      c_df$end_week <- mod_data$end_week
      c_df$region <- mod_data$region 
      c_df$date <- as.Date(paste0(mod_data$end_week, "-1"), format = "%Y-%W-%u")
      
      return(c_df)
    })
    
    rm(mod_data)
    gc()
    return(c_df_part)
  })
  
  rownames(coef_df) <- NULL
  
  # Map terms using the central dictionary
  coef_df$term <- sapply(coef_df$term, function(x) {
    if (x %in% names(config$term_labels)) {
      return(config$term_labels[[x]])
    } else if (grepl("W\\*", x)) { 
      base_var <- gsub("W\\*", "", x)
      clean_base <- ifelse(base_var %in% names(config$term_labels), config$term_labels[[base_var]], base_var)
      return(paste("Wx", clean_base))
    } else if (grepl("^Wx_", x)) {
      base_var <- gsub("^Wx_", "", x)
      clean_base <- ifelse(base_var %in% names(config$term_labels), config$term_labels[[base_var]], base_var)
      return(paste("Wx", clean_base))
    } else {
      return(x)
    }
  })
  
  return(coef_df)
}

# ------------------------------------------------------------------------------
# UPDATED VISUALIZATION & TABLE FUNCTIONS (ADD TO analyze_results.r)
# ------------------------------------------------------------------------------

plot_comparative_timeseries <- function(coef_df, target_term, title, is_split = TRUE) {
  plot_data <- coef_df %>% filter(term == target_term)
  if (nrow(plot_data) == 0) return(NULL)
  
  # Categorize by significance
  plot_data <- plot_data %>% 
    mutate(significance = ifelse(p_value < 0.05, "p < 0.05", "p >= 0.05"))
  
  p <- ggplot(plot_data, aes(x = date, y = estimate, color = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_line(linewidth = 0.8, alpha = 0.6) +
    geom_point(aes(shape = significance), size = 2) +
    scale_shape_manual(values = c("p < 0.05" = 19, "p >= 0.05" = 4)) +
    theme_minimal() +
    labs(title = title, x = "Date", y = "Estimate", color = "Model", shape = "Significance")
  
  if (is_split && length(unique(plot_data$region)) > 1) {
    p <- p + facet_wrap(~ region, scales = "free_y")
  }
  return(p)
}

plot_summary_statistics <- function(summary_table, terms_to_plot = NULL) {
  plot_data <- summary_table %>% filter(term != "(Intercept)")
  if (!is.null(terms_to_plot)) plot_data <- plot_data %>% filter(term %in% terms_to_plot)
  if (nrow(plot_data) == 0) return(NULL)
  
  # Map significance to alpha (transparency)
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
  
  # Helper to append stars
  format_sig <- function(est, pval) {
    stars <- ifelse(pval < 0.01, "***", ifelse(pval < 0.05, "**", ifelse(pval < 0.1, "*", "")))
    paste0(round(est, 4), stars)
  }
  
  # 1. Cross-Model Comparison (Averaged across all regions)
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
  
  # 2. Regional Breakdown
  region_comp <- summary_stats %>%
    mutate(formatted_val = format_sig(mean_estimate, mean_p_value)) %>%
    select(region, term, model, formatted_val) %>%
    pivot_wider(names_from = region, values_from = formatted_val) %>%
    arrange(model, term)
  
  write_csv(region_comp, file.path(tables_dir, "table_2_regional_breakdown_with_stars.csv"))
  
  # Write legend to text file for reference
  writeLines("Significance codes: 0 '***' 0.01 '**' 0.05 '*' 0.1", file.path(tables_dir, "legend.txt"))
  
  cat("Compact presentation tables with significance stars saved to:", tables_dir, "\n")
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

# ------------------------------------------------------------------------------
# 6. SENSITIVITY VISUALIZATION
# ------------------------------------------------------------------------------
plot_radius_sensitivity <- function(all_radiuses_df, output_dir) {
  sens_dir <- file.path(output_dir, "sensitivity_plots")
  if (!dir.exists(sens_dir)) dir.create(sens_dir, recursive = TRUE)
  
  plot_data <- all_radiuses_df %>% filter(term != "(Intercept)")
  unique_terms <- unique(plot_data$term)
  
  cat("\nGenerating sensitivity plots...\n")
  
  for (t in unique_terms) {
    sub_data <- plot_data %>% 
      filter(term == t) %>%
      mutate(is_sig = ifelse(mean_p_value < 0.05, "Sig (p<0.05)", "Not Sig"))
    
    if (nrow(sub_data) == 0) next
    
    p <- ggplot(sub_data, aes(x = as.factor(radius), y = mean_estimate, color = model, group = model)) +
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
    
    if (length(unique(sub_data$region)) > 1) {
      p <- p + facet_wrap(~ region, scales = "free_y")
    }
    
    safe_term <- gsub("[^A-Za-z0-9]", "_", t)
    ggsave(file.path(sens_dir, paste0("sensitivity_", safe_term, ".png")), plot = p, width = 10, height = 6, bg = "white")
  }
  
  cat("Sensitivity plots saved to:", sens_dir, "\n")
}

plot_coefficient_maps <- function(summary_table, shp_path, nuts_path, 
                                  target_nuts_level = "nuts1", target_model = "SDM", 
                                  target_term = "Spatial Lag (rho)") {
  nuts_data <- as.data.frame(fromJSON(nuts_path)$resultset) %>% 
    select(PRO_COM, contains("DEN")) %>% 
    rename(nuts1 = DEN_RIP, nuts2 = DEN_REG, nuts3 = DEN_UTS)
  
  shp_data <- st_read(shp_path, quiet = TRUE) %>% left_join(nuts_data, by = c("PRO_COM" = "PRO_COM"))
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

plot_summary_statistics <- function(summary_table, terms_to_plot = NULL) {
  plot_data <- summary_table %>% filter(term != "(Intercept)")
  if (!is.null(terms_to_plot)) plot_data <- plot_data %>% filter(term %in% terms_to_plot)
  if (nrow(plot_data) == 0) return(NULL)
  
  p <- ggplot(plot_data, aes(y = reorder(model, mean_estimate), x = mean_estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_linerange(aes(xmin = mean_estimate - sd_estimate, xmax = mean_estimate + sd_estimate), linewidth = 1.2, alpha = 0.6) +
    geom_point(size = 3.5) +
    geom_point(aes(x = median_estimate), size = 1.5, shape = 21, fill = "white", stroke = 1) +
    theme_minimal() +
    labs(
      title = "Coefficient Stability and Central Tendency",
      subtitle = "Solid dot = Mean | White dot = Median | Lines = ±1 SD",
      x = "Coefficient Estimate", y = "Model Specification"
    ) +
    theme(legend.position = "none", strip.text = element_text(face = "bold", size = 11),
          axis.text.y = element_text(face = "bold", size = 10), panel.grid.minor = element_blank())
  
  if ("region" %in% colnames(plot_data) && length(unique(plot_data$region)) > 1) {
    p <- p + facet_grid(region ~ term, scales = "free_x")
  } else {
    p <- p + facet_wrap(~ term, scales = "free_x", ncol = 2)
  }
  return(p)
}

# ------------------------------------------------------------------------------
# 5. BATCH VISUALIZATION GENERATOR
# ------------------------------------------------------------------------------

generate_all_visualizations <- function(stats_data, summary_stats, output_dir, shp_path, nuts_path, estimation_granularity) {
  
  # Create structured directories
  dirs <- list(
    timeseries = file.path(output_dir, "plots", "timeseries"),
    maps       = file.path(output_dir, "plots", "maps"),
    summaries  = file.path(output_dir, "plots", "summaries")
  )
  lapply(dirs, function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE))
  
  unique_terms <- unique(stats_data$term)
  unique_models <- unique(stats_data$model)
  
  cat("\nGenerating all plots. This may take a moment...\n")
  
  for (t in unique_terms) {
    if (t == "(Intercept)") next
    
    safe_term <- gsub("[^A-Za-z0-9]", "_", t)
    
    # 1. Timeseries Plots (Comparing models for a single term)
    p_ts <- plot_comparative_timeseries(stats_data, t, paste("Time Series:", t), is_split = TRUE)
    if (!is.null(p_ts)) {
      ggsave(file.path(dirs$timeseries, paste0("ts_", safe_term, ".png")), plot = p_ts, width = 12, height = 8, bg = "white")
    }
    
    # 2. Summary Statistics Plots (Comparing models for a single term)
    p_sum <- plot_summary_statistics(summary_stats, terms_to_plot = t)
    if (!is.null(p_sum)) {
      ggsave(file.path(dirs$summaries, paste0("summary_", safe_term, ".png")), plot = p_sum, width = 10, height = 8, bg = "white")
    }
    
    # 3. Maps (One per term AND per model)
    for (m in unique_models) {
      safe_model <- gsub("[^A-Za-z0-9]", "_", m)
      
      tryCatch({
        map_out <- plot_coefficient_maps(summary_stats, shp_path, nuts_path, 
                                         target_nuts_level = estimation_granularity, 
                                         target_model = m, target_term = t)
        
        if (!is.null(map_out$plot)) {
          ggsave(file.path(dirs$maps, paste0("map_", safe_model, "_", safe_term, "_", estimation_granularity, ".png")), 
                 plot = map_out$plot, width = 10, height = 8, bg = "white")
        }
      }, error = function(e) {
        # Catch cases where mapping fails (e.g., shapefile mismatch for a specific region subset)
      })
    }
  }
  
  # 4. Overall Summary Plot (All terms together)
  p_all_sum <- plot_summary_statistics(summary_stats, terms_to_plot = NULL)
  if (!is.null(p_all_sum)) {
    ggsave(file.path(output_dir, "plots", "summary_ALL_TERMS.png"), plot = p_all_sum, width = 16, height = 12, bg = "white")
  }
  
  cat("All plots successfully generated and organized in:", file.path(output_dir, "plots"), "\n")
}