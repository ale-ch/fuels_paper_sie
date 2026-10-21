library(tidyverse)
library(purrr)
library(sf)

geo_data <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/data/geo_data/geo_data.RDS")
data_path <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/results_run_nuts3_42_20261016_214911.RDS"
# base_out_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation/maps"
base_out_dir <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation/maps2"

res_df <- readRDS(data_path)
bolzano_df <- res_df %>% 
  filter(region == "Trento") %>% 
  mutate(
    region = "Bolzano/Bozen"
  )

res_df <- bind_rows(res_df, bolzano_df)

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
avg_by_nuts3$`Bolzano/Bozen` <- avg_by_nuts3$Trento

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


plot_conference_map_clipped <- function(plot_data, model_name, term_name, nuts_level) {
  
  # Define dynamic limits based on 5th and 95th percentiles
  lims <- quantile(plot_data$num_value, probs = c(0.05, 0.95), na.rm = TRUE)
  
  ggplot(plot_data) +
    geom_sf(aes(fill = num_value), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "plasma", 
      direction = -1,
      na.value = "grey85",
      limits = lims,
      oob = scales::squish, # Forces outliers to the ends of the color scale
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
      fill = "Asymmetry Gap"
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




#### SPATIAL MULTIPLIERS ####

# 2. Compute Regional Multipliers (Grouping by region, not end_date)
mult_by_nuts3 <- res_df %>%
  filter(model %in% c("GNS", "SAR", "SARAR", "SDM"), 
         term == "Spatial Lag (lambda)", 
         !is.na(term), 
         abs(estimate) < 0.99) %>%
  mutate(
    multiplier = abs(1 / (1 - estimate)),
    term = "Spatial Multiplier" # Standardize term name for plotting
  ) %>%
  group_by(model, term, region) %>%
  summarize(num_value = median(multiplier, na.rm = TRUE), .groups = "drop") %>%
  rename(nuts3 = region)

top5_df <- mult_by_nuts3 %>% 
  filter(
    model == "SAR"
  ) %>% 
  arrange(desc(num_value)) %>% 
  head(5)

bottom5_df <- mult_by_nuts3 %>% 
  filter(
    model == "SAR"
  ) %>% 
  arrange(desc(num_value)) %>% 
  tail(5)

bind_rows(top5_df, bottom5_df) %>% write_csv("/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation_tables/topbot_spatial_multiplier.csv")



# 3. Join with NUTS hierarchy
geo_data_df <- geo_data %>% 
  st_drop_geometry() %>% 
  select(nuts1, nuts2, nuts3) %>% 
  distinct()

mult_by_nuts3 <- left_join(mult_by_nuts3, geo_data_df, by = "nuts3")

# 4. Aggregate to NUTS-2 and NUTS-1
mult_by_nuts2 <- mult_by_nuts3 %>%
  group_by(term, model, nuts2) %>%
  summarize(num_value = mean(num_value, na.rm = TRUE), .groups = "drop")

mult_by_nuts1 <- mult_by_nuts3 %>%
  group_by(term, model, nuts1) %>%
  summarize(num_value = mean(num_value, na.rm = TRUE), .groups = "drop")

# 5. Aggregate spatial polygons
geo_nuts3 <- geo_data %>% group_by(nuts3) %>% summarize(geometry = st_union(geometry), .groups = "drop")
geo_nuts2 <- geo_data %>% group_by(nuts2) %>% summarize(geometry = st_union(geometry), .groups = "drop")
geo_nuts1 <- geo_data %>% group_by(nuts1) %>% summarize(geometry = st_union(geometry), .groups = "drop")

# 6. Define Plotting Function
plot_conference_map <- function(plot_data, model_name, term_name, nuts_level) {
  ggplot(plot_data) +
    geom_sf(aes(fill = num_value), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "plasma", 
      direction = -1,
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
      fill = "Multiplier Estimate"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 5, t = 10)),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30", margin = margin(b = 15)),
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

# 7. Iterate, Plot, and Save
combinations <- mult_by_nuts3 %>% distinct(model, term)

walk2(combinations$model, combinations$term, function(c_model, c_term) {
  
  safe_term <- str_replace_all(c_term, "[^A-Za-z0-9_.-]", "_")
  
  dir_n1 <- file.path(base_out_dir, c_model, "NUTS1")
  dir_n2 <- file.path(base_out_dir, c_model, "NUTS2")
  dir_n3 <- file.path(base_out_dir, c_model, "NUTS3")
  
  walk(c(dir_n1, dir_n2, dir_n3), ~dir.create(.x, recursive = TRUE, showWarnings = FALSE))
  
  # -- NUTS-3 --
  data_n3 <- geo_nuts3 %>% left_join(mult_by_nuts3 %>% filter(model == c_model), by = "nuts3")
  p3 <- plot_conference_map(data_n3, c_model, c_term, "NUTS-3")
  ggsave(file.path(dir_n3, paste0(safe_term, ".png")), plot = p3, width = 8, height = 8, dpi = 300)
  
  # -- NUTS-2 --
  data_n2 <- geo_nuts2 %>% left_join(mult_by_nuts2 %>% filter(model == c_model), by = "nuts2")
  p2 <- plot_conference_map(data_n2, c_model, c_term, "NUTS-2")
  ggsave(file.path(dir_n2, paste0(safe_term, ".png")), plot = p2, width = 8, height = 8, dpi = 300)
  
  # -- NUTS-1 --
  data_n1 <- geo_nuts1 %>% left_join(mult_by_nuts1 %>% filter(model == c_model), by = "nuts1")
  p1 <- plot_conference_map(data_n1, c_model, c_term, "NUTS-1")
  ggsave(file.path(dir_n1, paste0(safe_term, ".png")), plot = p1, width = 8, height = 8, dpi = 300)
})

cat("Multiplier maps successfully generated and saved to:", base_out_dir, "\n")




#### ASYMMETRY ####
lambda_df <- res_df %>%
  filter(term == "Spatial Lag (lambda)") %>%
  select(model, region, end_week, radius_m, lambda = estimate, lambda_se = std_error, lambda_p = p_value)


brent_df <- res_df %>%
  filter(term %in% c("brent_diff_pos", "brent_diff_neg")) %>%
  select(model, region, end_week, end_date, window, radius_m, 
         term, beta = estimate, beta_se = std_error, beta_p = p_value)

impacts_df <- brent_df %>%
  left_join(lambda_df, by = c("model", "region", "end_week", "radius_m")) %>%
  mutate(
    lambda = replace_na(lambda, 0),
    valid_mult = abs(lambda) < 0.99,
    Direct_Impact = ifelse(valid_mult, beta, NA),
    Total_Impact = ifelse(valid_mult, beta / (1 - lambda), NA),
    Indirect_Impact = Total_Impact - Direct_Impact,
    Total_SE = ifelse(valid_mult, 
                      sqrt((beta_se^2 / (1 - lambda)^2) + ((beta^2 * lambda_se^2) / (1 - lambda)^4)), 
                      NA),
    Total_pval = 2 * (1 - pnorm(abs(Total_Impact / Total_SE)))
  ) %>%
  filter(!is.na(Total_Impact))

# 2. Compute Regional Asymmetry (Grouping by region)
asymmetry_by_nuts3 <- impacts_df %>%
  filter(model == "SAR") %>%
  select(region, end_date, term, Total_Impact, Total_SE) %>%
  pivot_wider(names_from = term, values_from = c(Total_Impact, Total_SE)) %>%
  mutate(
    asym_gap = Total_Impact_brent_diff_pos - abs(Total_Impact_brent_diff_neg)
  ) %>%
  group_by(region) %>%
  summarize(num_value = mean(asym_gap, na.rm = TRUE), .groups = "drop") %>%
  rename(nuts3 = region) %>%
  mutate(model = "SAR", term = "Asymmetry Gap")

top5_df <- asymmetry_by_nuts3 %>% 
  filter(
    model == "SAR"
  ) %>% 
  arrange(desc(num_value)) %>% 
  head(5)

bottom5_df <- asymmetry_by_nuts3 %>% 
  filter(
    model == "SAR"
  ) %>% 
  arrange(desc(num_value)) %>% 
  tail(5)

bind_rows(top5_df, bottom5_df) %>% write_csv("/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/presentation_tables/topbot_asymmetry.csv")


# 3. Join with NUTS hierarchy
geo_data_df <- geo_data %>% 
  st_drop_geometry() %>% 
  select(nuts1, nuts2, nuts3) %>% 
  distinct()

asymmetry_by_nuts3 <- left_join(asymmetry_by_nuts3, geo_data_df, by = "nuts3")

# 4. Aggregate to NUTS-2 and NUTS-1
asymmetry_by_nuts2 <- asymmetry_by_nuts3 %>%
  group_by(term, model, nuts2) %>%
  summarize(num_value = mean(num_value, na.rm = TRUE), .groups = "drop")

asymmetry_by_nuts1 <- asymmetry_by_nuts3 %>%
  group_by(term, model, nuts1) %>%
  summarize(num_value = mean(num_value, na.rm = TRUE), .groups = "drop")

# 5. Aggregate spatial polygons
geo_nuts3 <- geo_data %>% group_by(nuts3) %>% summarize(geometry = st_union(geometry), .groups = "drop")
geo_nuts2 <- geo_data %>% group_by(nuts2) %>% summarize(geometry = st_union(geometry), .groups = "drop")
geo_nuts1 <- geo_data %>% group_by(nuts1) %>% summarize(geometry = st_union(geometry), .groups = "drop")

# 6. Plotting Function
plot_conference_map <- function(plot_data, model_name, term_name, nuts_level) {
  ggplot(plot_data) +
    geom_sf(aes(fill = num_value), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "plasma", 
      direction = -1,
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
      fill = "Asymmetry Gap"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 5, t = 10)),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30", margin = margin(b = 15)),
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = "white", color = NA)
    )
}

plot_conference_map_clipped <- function(plot_data, model_name, term_name, nuts_level) {
  
  # Define dynamic limits based on 5th and 95th percentiles
  lims <- quantile(plot_data$num_value, probs = c(0.05, 0.95), na.rm = TRUE)
  
  ggplot(plot_data) +
    geom_sf(aes(fill = num_value), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(
      option = "plasma", 
      direction = -1,
      na.value = "grey85",
      limits = lims,
      oob = scales::squish, # Forces outliers to the ends of the color scale
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
      fill = "Asymmetry Gap"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 5, t = 10)),
      plot.subtitle = element_text(hjust = 0.5, color = "grey30", margin = margin(b = 15)),
      legend.position = "bottom",
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = "white", color = NA)
    )
}


# 7. Create Directories and Plot
walk(c("NUTS1", "NUTS2", "NUTS3"), function(level) {
  dir.create(file.path(base_out_dir, "SAR", level), recursive = TRUE, showWarnings = FALSE)
})

# -- NUTS-3 --
data_n3 <- geo_nuts3 %>% left_join(asymmetry_by_nuts3, by = "nuts3")
p3 <- plot_conference_map(data_n3, "SAR", "Asymmetry Gap", "NUTS-3")
ggsave(file.path(base_out_dir, "SAR", "NUTS3", "Asymmetry_Gap.png"), plot = p3, width = 8, height = 8, dpi = 300)
#p3_2 <- plot_conference_map(
#  data_n3, 
#  "SAR", "Asymmetry Gap", "NUTS-3"
#  )


p3_2 <- plot_conference_map_clipped(data_n3, "SAR", "Asymmetry Gap", "NUTS-3")
ggsave(file.path(base_out_dir, "SAR", "NUTS3", "Asymmetry_Gap2.png"), plot = p3_2, width = 8, height = 8, dpi = 300)

# -- NUTS-2 --
data_n2 <- geo_nuts2 %>% left_join(asymmetry_by_nuts2, by = "nuts2")
p2 <- plot_conference_map(data_n2, "SAR", "Asymmetry Gap", "NUTS-2")
ggsave(file.path(base_out_dir, "SAR", "NUTS2", "Asymmetry_Gap.png"), plot = p2, width = 8, height = 8, dpi = 300)

# -- NUTS-1 --
data_n1 <- geo_nuts1 %>% left_join(asymmetry_by_nuts1, by = "nuts1")
p1 <- plot_conference_map(data_n1, "SAR", "Asymmetry Gap", "NUTS-1")
ggsave(file.path(base_out_dir, "SAR", "NUTS1", "Asymmetry_Gap2.png"), plot = p1, width = 8, height = 8, dpi = 300)

cat("Asymmetry maps successfully generated and saved to:", base_out_dir, "\n")