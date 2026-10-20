library(tidyverse)

# 1. Load data and compute impacts
data_path <- "/Volumes/T7 Shield/FRES/fuels_data/results/run_nuts3_42_05_STEP/results_run_nuts3_42_20261016_214911.RDS"
res_df <- readRDS(data_path)

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

mult_df <- impacts_df %>%
  filter(model %in% c("GNS", "SAR", "SARAR", "SDM"), term == "brent_diff_pos") %>%
  mutate(multiplier = 1 / (1 - lambda)) %>%
  group_by(model, end_date) %>%
  summarize(mean_mult = median(multiplier, na.rm = TRUE), .groups = "drop")

# Aggregate across NUTS-3 provinces by date, model, and term
nat_impacts <- impacts_df %>%
  group_by(model, term, end_date) %>%
  summarize(
    Direct = mean(Direct_Impact, na.rm = TRUE),
    Indirect = mean(Indirect_Impact, na.rm = TRUE),
    Total = mean(Total_Impact, na.rm = TRUE),
    sig_share = mean(Total_pval < 0.05, na.rm = TRUE),
    .groups = "drop"
  )



mult_df2 <- res_df %>%
  filter(model %in% c("GNS", "SAR", "SARAR", "SDM"), term == "Spatial Lag (lambda)", !is.na(term), abs(estimate) < 0.99) %>%
  select(model, end_date, region, end_week, radius_m, lambda = estimate, lambda_se = std_error, lambda_p = p_value) %>% 
  mutate(
    multiplier = abs(1 / (1 - lambda))
  ) %>%
  group_by(model, end_date) %>%
  summarize(mean_mult = median(multiplier, na.rm = TRUE), .groups = "drop")

# ------------------------------------------------------------------------------
# PLOT 1: Direct vs. Indirect vs. Total Impact Decomposition (Target Model: SAR)
# ------------------------------------------------------------------------------
p1_data <- nat_impacts %>%
  filter(model == "SAR") %>%
  pivot_longer(cols = c(Direct, Indirect, Total), names_to = "Impact_Type", values_to = "Estimate") %>%
  mutate(
    Impact_Type = factor(Impact_Type, levels = c("Total", "Indirect", "Direct")),
    term_label = ifelse(term == "brent_diff_pos", "Positive Brent Shock (brent_diff_pos)", "Negative Brent Shock (brent_diff_neg)")
  )

p1 <- ggplot(p1_data, aes(x = end_date, y = Estimate, color = Impact_Type, linetype = Impact_Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ term_label, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Total" = "#1D396B", "Indirect" = "#EF8F3A", "Direct" = "#E13A3A")) +
  scale_linetype_manual(values = c("Total" = "solid", "Indirect" = "solid", "Direct" = "dotted")) +
  labs(
    title = "Spatial Impact Decomposition: Direct vs. Indirect Spillovers",
    subtitle = "Mean across NUTS-3 Provinces | Model: SAR | Rolling Window: 52 Weeks",
    x = "Rolling Window End Date",
    y = "Impact Estimate",
    color = "Impact Component",
    linetype = "Impact Component"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#002B52"),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "right"
  )

ggsave("plot1_impact_decomposition.png", p1, width = 10, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# PLOT 2: Total Impact Comparison Across Lag Models (GNS, SAR, SARAR, SDM)
# ------------------------------------------------------------------------------
p2_data <- nat_impacts %>%
  filter(model %in% c("GNS", "SAR", "SARAR", "SDM")) %>%
  mutate(is_sig = ifelse(sig_share >= 0.5, "p < 0.05 (Majority)", "p >= 0.05"))

p2_pos <- ggplot(filter(p2_data, term == "brent_diff_pos"), 
                 aes(x = end_date, y = Total, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = is_sig), size = 1.8) +
  scale_shape_manual(values = c("p < 0.05 (Majority)" = 16, "p >= 0.05" = 4)) +
  labs(
    title = "Total Spatial Impact: brent_diff_pos",
    subtitle = "Comparison Across Spatial Lag Specifications (beta / (1 - lambda))",
    x = "Rolling Window End Date",
    y = "Total Impact Estimate",
    color = "Model",
    shape = "Significance"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#002B52"))

ggsave("plot2_total_impact_pos_models.png", p2_pos, width = 10, height = 5.5, dpi = 300)

p2_neg <- ggplot(filter(p2_data, term == "brent_diff_neg"), 
                 aes(x = end_date, y = Total, color = model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = is_sig), size = 1.8) +
  scale_shape_manual(values = c("p < 0.05 (Majority)" = 16, "p >= 0.05" = 4)) +
  labs(
    title = "Total Spatial Impact: brent_diff_neg",
    subtitle = "Comparison Across Spatial Lag Specifications (beta / (1 - lambda))",
    x = "Rolling Window End Date",
    y = "Total Impact Estimate",
    color = "Model",
    shape = "Significance"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#002B52"))

ggsave("plot2_total_impact_neg_models.png", p2_neg, width = 10, height = 5.5, dpi = 300)

# ------------------------------------------------------------------------------
# PLOT 3: Asymmetric Pass-Through ("Rockets and Feathers") on Total Impacts
# ------------------------------------------------------------------------------
asymmetry_df <- impacts_df %>%
  filter(model == "SAR") %>%
  select(region, end_date, term, Total_Impact, Total_SE) %>%
  pivot_wider(names_from = term, values_from = c(Total_Impact, Total_SE)) %>%
  mutate(
    asym_gap = Total_Impact_brent_diff_pos - abs(Total_Impact_brent_diff_neg),
    asym_se  = sqrt(Total_SE_brent_diff_pos^2 + Total_SE_brent_diff_neg^2),
    z_score  = asym_gap / asym_se,
    p_val    = 2 * (1 - pnorm(abs(z_score)))
  ) %>%
  group_by(end_date) %>%
  summarize(
    mean_gap = mean(asym_gap, na.rm = TRUE),
    se_gap   = sd(asym_gap, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  )

p3 <- ggplot(asymmetry_df, aes(x = end_date, y = mean_gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin = mean_gap - 1.96 * se_gap, ymax = mean_gap + 1.96 * se_gap), 
              fill = "#1D396B", alpha = 0.2) +
  geom_line(color = "#1D396B", linewidth = 0.9) +
  labs(
    title = "Asymmetric Price Transmission (Rockets and Feathers)",
    subtitle = "Difference: Total Impact (Positive Shock) - |Total Impact (Negative Shock)| (SAR Model)",
    x = "Rolling Window End Date",
    y = "Asymmetry Gap (95% CI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#002B52"))

ggsave("plot3_asymmetry_gap.png", p3, width = 10, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# PLOT 4: Spatial Multiplier Amplification Factor (1 / (1 - lambda)) Over Time
# ------------------------------------------------------------------------------
p4 <- ggplot(mult_df, aes(x = end_date, y = mean_mult, color = model)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Spatial Network Amplification Factor: 1 / (1 - lambda)",
    subtitle = "Median Amplification of National Shocks via Local Competition Across NUTS-3",
    x = "Rolling Window End Date",
    y = "Spatial Multiplier",
    color = "Model"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#002B52"))

ggsave("plot4_spatial_multiplier.png", p4, width = 10, height = 5, dpi = 300)



p5 <- ggplot(mult_df2 %>% filter(model == "SAR"), aes(x = end_date, y = mean_mult)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Spatial Network Amplification Factor: 1 / (1 - lambda)",
    subtitle = "Median Amplification of National Shocks via Local Competition (NUTS 0, SAR)",
    x = "Rolling Window End Date",
    y = "Spatial Multiplier"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#002B52"))

ggsave("plot4_spatial_multiplier_SAR.png", p5, width = 10, height = 5, dpi = 300)

