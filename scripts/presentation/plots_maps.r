library(tidyverse)
library(purrr)
library(sf)

geo_data <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/data/geo_data/geo_data.RDS")
data_path <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/results_run_nuts3_42_20261016_214911.RDS"
base_out_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation/maps"
res_df <- readRDS(data_path)

tables_paths <- c(
  "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation_tables/legend.txt",
  "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation_tables/table_1_model_comparison_with_stars.csv",
  "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation_tables/table_2_regional_breakdown_with_stars.csv"
)

tables <- map(tables_paths, read_csv)

tables[2:3] <- lapply(tables[2:3], function(x) {
  x %>% filter(!is.na(term))
})

avg_by_nuts3 <- tables[[3]]

avg_by_nuts3_long <- pivot_longer(
  avg_by_nuts3,
  cols = 3:length(avg_by_nuts3),
  names_to = "region_name"
  )


geo_data_df <- geo_data %>% 
  st_drop_geometry() 
  

avg_by_nuts3_long <- left_join(
  avg_by_nuts3_long %>% rename(nuts3 = region_name), 
  geo_data_df %>% select(nuts1, nuts2, nuts3) %>% filter(!duplicated(nuts3)),
  by = "nuts3"
  ) %>% 
  select(
    term, model, value, nuts3, nuts2, nuts1
  )





library(stringr)
library(ggplot2)

# 1. Strip stars and convert value to numeric
avg_by_nuts3_long <- avg_by_nuts3_long %>%
  mutate(
    # Extracts the first numeric value, ignoring stars or parenthesis
    num_value = as.numeric(str_extract(value, "^-?\\d+\\.?\\d*"))
  ) %>%
  filter(!is.na(num_value)) # Drop standard error rows if they were parsed as NA

# 2. Aggregate values to NUTS-2 and NUTS-1 (using mean of NUTS-3 regions)
avg_by_nuts2 <- avg_by_nuts3_long %>%
  group_by(term, model, nuts2) %>%
  summarize(num_value = mean(num_value, na.rm = TRUE), .groups = "drop")

avg_by_nuts1 <- avg_by_nuts3_long %>%
  group_by(term, model, nuts1) %>%
  summarize(num_value = mean(num_value, na.rm = TRUE), .groups = "drop")

# 3. Aggregate spatial polygons from NUTS-3 to NUTS-2 and NUTS-1
geo_nuts3 <- geo_data %>% 
  group_by(nuts3) %>% 
  summarize(geometry = st_union(geometry), .groups = "drop")

geo_nuts2 <- geo_data %>% 
  group_by(nuts2) %>% 
  summarize(geometry = st_union(geometry), .groups = "drop")

geo_nuts1 <- geo_data %>% 
  group_by(nuts1) %>% 
  summarize(geometry = st_union(geometry), .groups = "drop")

# 2. Define Conference-Ready Plotting Function
plot_conference_map <- function(plot_data, model_name, term_name, nuts_level) {
  ggplot(plot_data) +
    geom_sf(aes(fill = num_value), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "plasma", 
      direction = -1,     # Reverses the scale: higher values = darker colors
      na.value = "grey85",
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 15,
        barheight = 0.5,
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    theme_void(base_size = 14) +
    labs(
      title = paste(nuts_level, "-", model_name),
      subtitle = term_name,
      fill = "Coefficient Estimate"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 5, t = 10)),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30", margin = margin(b = 15)),
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# 3. Get unique combinations of models and terms
combinations <- avg_by_nuts3_long %>%
  distinct(model, term)

# 4. Iterate, Plot, and Save
walk2(combinations$model, combinations$term, function(c_model, c_term) {
  
  # Clean term string for safe file naming
  safe_term <- str_replace_all(c_term, "[^A-Za-z0-9_.-]", "_")
  
  # Define subdirectories
  dir_n1 <- file.path(base_out_dir, c_model, "NUTS1")
  dir_n2 <- file.path(base_out_dir, c_model, "NUTS2")
  dir_n3 <- file.path(base_out_dir, c_model, "NUTS3")
  
  # Create directories safely
  walk(c(dir_n1, dir_n2, dir_n3), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))
  
  # -- NUTS-3 --
  data_n3 <- geo_nuts3 %>% 
    left_join(avg_by_nuts3_long %>% filter(model == c_model, term == c_term), by = "nuts3")
  p3 <- plot_conference_map(data_n3, c_model, c_term, "NUTS-3")
  ggsave(file.path(dir_n3, paste0(safe_term, ".png")), plot = p3, width = 8, height = 8, dpi = 300)
  
  # -- NUTS-2 --
  data_n2 <- geo_nuts2 %>% 
    left_join(avg_by_nuts2 %>% filter(model == c_model, term == c_term), by = "nuts2")
  p2 <- plot_conference_map(data_n2, c_model, c_term, "NUTS-2")
  ggsave(file.path(dir_n2, paste0(safe_term, ".png")), plot = p2, width = 8, height = 8, dpi = 300)
  
  # -- NUTS-1 --
  data_n1 <- geo_nuts1 %>% 
    left_join(avg_by_nuts1 %>% filter(model == c_model, term == c_term), by = "nuts1")
  p1 <- plot_conference_map(data_n1, c_model, c_term, "NUTS-1")
  ggsave(file.path(dir_n1, paste0(safe_term, ".png")), plot = p1, width = 8, height = 8, dpi = 300)
  
})

cat("Maps successfully generated and saved to:", base_out_dir, "\n")