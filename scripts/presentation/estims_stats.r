library(tidyverse)

# 1. Isolate parameters
lambda_df <- res_df %>%
  filter(term == "Spatial Lag (lambda)") %>%
  select(model, region, end_week, lambda = estimate, lambda_p = p_value)

brent_df <- res_df %>%
  filter(term %in% c("brent_diff_pos", "brent_diff_neg")) %>%
  select(model, region, end_week, term, beta = estimate, beta_se = std_error, beta_p = p_value)

# 2. Compute Summary Diagnostics Table
summary_stats <- lambda_df %>%
  group_by(model) %>%
  summarize(
    total_windows     = n(),
    pct_stable        = mean(abs(lambda) < 1.0, na.rm = TRUE) * 100,
    pct_sig_lambda    = mean(lambda_p < 0.05, na.rm = TRUE) * 100,
    median_lambda     = median(lambda[abs(lambda) < 1.0], na.rm = TRUE),
    median_multiplier = median(1 / (1 - lambda[abs(lambda) < 1.0]), na.rm = TRUE),
    iqr_multiplier    = IQR(1 / (1 - lambda[abs(lambda) < 1.0]), na.rm = TRUE)
  )

print("--- SUMMARY DIAGNOSTICS TABLE ---")
print(summary_stats)

# 3. Compute Rockets & Feathers Asymmetry Share
asym_stats <- brent_df %>%
  pivot_wider(names_from = term, values_from = c(beta, beta_se, beta_p)) %>%
  left_join(lambda_df, by = c("model", "region", "end_week")) %>%
  filter(abs(lambda) < 0.99) %>%
  mutate(
    tot_pos = beta_brent_diff_pos / (1 - lambda),
    tot_neg = beta_brent_diff_neg / (1 - lambda),
    diff    = tot_pos - abs(tot_neg),
    se_diff = sqrt(beta_se_brent_diff_pos^2 + beta_se_brent_diff_neg^2) / (1 - lambda),
    z_score = diff / se_diff,
    p_val   = 2 * (1 - pnorm(abs(z_score))),
    is_rockets = (tot_pos > abs(tot_neg)) & (p_val < 0.05)
  ) %>%
  group_by(model) %>%
  summarize(
    pct_windows_rockets_feathers = mean(is_rockets, na.rm = TRUE) * 100
  )

print("--- ASYMMETRY SHARE (%) ---")
print(asym_stats)