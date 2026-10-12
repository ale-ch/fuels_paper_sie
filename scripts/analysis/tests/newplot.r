plot_summary_statistics <- function(summary_table, terms_to_plot = NULL) {
  require(ggplot2)
  require(dplyr)
  
  plot_data <- summary_table %>% filter(term != "(Intercept)")
  
  # Filter to essential variables to prevent facet overcrowding
  if (!is.null(terms_to_plot)) {
    plot_data <- plot_data %>% filter(term %in% terms_to_plot)
  }
  
  p <- ggplot(plot_data, aes(y = reorder(model, mean_estimate), x = mean_estimate, color = model)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_linerange(aes(xmin = mean_estimate - sd_estimate, 
                       xmax = mean_estimate + sd_estimate), 
                   linewidth = 1.2, alpha = 0.6) +
    geom_point(size = 3.5) +
    geom_point(aes(x = median_estimate), size = 1.5, shape = 21, fill = "white", stroke = 1) +
    scale_x_continuous(n.breaks = 4) + # Limit x-axis labels
    theme_minimal() +
    labs(
      title = "Coefficient Stability and Central Tendency",
      subtitle = "Solid dot = Mean | White dot = Median | Lines = ±1 SD",
      x = "Coefficient Estimate",
      y = "Model Specification"
    ) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10),
      axis.text.y = element_text(face = "bold", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # Rotate x-axis text
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1, "lines") # Add breathing room between facets
    )
  
  # Use facet_wrap instead of grid to allow completely independent x-scales per panel
  if ("nuts0" %in% colnames(plot_data) && length(unique(plot_data$nuts0)) > 1) {
    n_regions <- length(unique(plot_data$nuts0))
    p <- p + facet_wrap(~ term + nuts0, scales = "free_x", ncol = n_regions)
  } else {
    p <- p + facet_wrap(~ term, scales = "free_x", ncol = 2)
  }
  
  print(p)
  return(p)
}


# Example execution:
key_terms <- c("Spatial Lag (rho)", "Time Lag (Gasoline)", "Time Lag (Brent Δ+)", "Time Lag (Brent Δ-)")
p_summary <- plot_summary_statistics(pipeline_results$summary_stats, terms_to_plot = key_terms)
