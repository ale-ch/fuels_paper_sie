# Dynamic panel specification:
# y_it = gasoline price at pump i, week t
# Temporal lags are handled through the panel structure.

base_formula <- mean_price_gasoline_self ~
  lag(mean_price_gasoline_self, 1) +
  lag(brent_price_mean, 1)


# Convert to panel data
df_panel <- pdata.frame(
  df,
  index = c("id_pump", "time_index")
)


# 1. SLX
# Spatial lags of explanatory variables (WX)
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


# 2. SEM
# Spatially correlated error term
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


# 3. SDEM
# Spatial lags of X (WX) + spatially correlated error
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


# 4. SAR
# Spatial lag of dependent variable (Wy)
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


# 5. SDM
# Spatial lag of dependent variable (Wy) + spatial lags of X (WX)
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


# 6. SARAR / SAC
# Spatial lag of dependent variable (Wy) + spatially correlated error
run_model_sarar <- function(df_panel, W, form = base_formula) {
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


# 7. GNS
# Wy + WX + spatially correlated error
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


# Run models
models <- list(
  SLX   = run_model_slx(df_panel, W),
  SEM   = run_model_sem(df_panel, W),
  SDEM  = run_model_sdem(df_panel, W),
  SAR   = run_model_sar(df_panel, W),
  SDM   = run_model_sdm(df_panel, W),
  SARAR = run_model_sarar(df_panel, W),
  GNS   = run_model_gns(df_panel, W)
)

