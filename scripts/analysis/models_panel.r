# Base formula incorporating temporal lags
base_formula <- mean_price_gasoline_self ~ lag(mean_price_gasoline_self, 1) + lag(brent_price_mean, 1)

# 1. SLX (Spatial Lag of X)
# Includes spatial lags of independent variables (WX) only.
run_model_slx <- function(df_panel, W, form = base_formula) {
  spgm(
    formula = form, 
    data = df_panel, 
    listw = W, 
    lag = FALSE, 
    spatial.error = FALSE, 
    Durbin = TRUE, 
    method = "w2sls"
  )
}

# 2. SLX WITH TIME-LAGGED INDEPENDENT VARIABLES
# Identical to SLX in spgm specification, but relies on the formula providing time lags.
run_model_slx_time_lags <- function(df_panel, W) {
  # Explicitly defining the time lags in the formula
  form_time_lags <- mean_price_gasoline_self ~ lag(mean_price_gasoline_self, 1) + lag(brent_price_mean, 1)
  spgm(
    formula = form_time_lags, 
    data = df_panel, 
    listw = W, 
    lag = FALSE, 
    spatial.error = FALSE, 
    Durbin = TRUE, 
    method = "w2sls"
  )
}

# 3. SEM (Spatial Error Model)
# Includes spatial lag of the error term (Wε) only.
run_model_sem <- function(df_panel, W, form = base_formula) {
  spgm(
    formula = form, 
    data = df_panel, 
    listw = W, 
    lag = FALSE, 
    spatial.error = TRUE, 
    Durbin = FALSE, 
    method = "w2sls"
  )
}

# 4. SDEM (Spatial Durbin Error Model)
# Includes spatial lag of independent variables (WX) and spatial error (Wε).
run_model_sdem <- function(df_panel, W, form = base_formula) {
  spgm(
    formula = form, 
    data = df_panel, 
    listw = W, 
    lag = FALSE, 
    spatial.error = TRUE, 
    Durbin = TRUE, 
    method = "w2sls"
  )
}

# 5. SAR (Spatial Autoregressive Model)
# Includes spatial lag of the dependent variable (Wy) only.
run_model_sar <- function(df_panel, W, form = base_formula) {
  spgm(
    formula = form, 
    data = df_panel, 
    listw = W, 
    lag = TRUE, 
    spatial.error = FALSE, 
    Durbin = FALSE, 
    method = "w2sls"
  )
}

# 6. SDM (Spatial Durbin Model)
# Includes spatial lag of dependent variable (Wy) and independent variables (WX).
run_model_sdm <- function(df_panel, W, form = base_formula) {
  spgm(
    formula = form, 
    data = df_panel, 
    listw = W, 
    lag = TRUE, 
    spatial.error = FALSE, 
    Durbin = TRUE, 
    method = "w2sls"
  )
}

# 7. SAC / SARAR (Spatial Autoregressive with Spatial Error)
# Includes spatial lag of dependent variable (Wy) and spatial error (Wε).
run_model_sac <- function(df_panel, W, form = base_formula) {
  spgm(
    formula = form, 
    data = df_panel, 
    listw = W, 
    lag = TRUE, 
    spatial.error = TRUE, 
    Durbin = FALSE, 
    method = "w2sls"
  )
}

# 8. GNS (Generalized Nested Spatial Model)
# Includes spatial lags of dependent variable (Wy), independent variables (WX), and spatial error (Wε).
run_model_gns <- function(df_panel, W, form = base_formula) {
  spgm(
    formula = form, 
    data = df_panel, 
    listw = W, 
    lag = TRUE, 
    spatial.error = TRUE, 
    Durbin = TRUE, 
    method = "w2sls"
  )
}