library(plm)

# Extract the target variable from your pdata.frame
price_var <- p_data$mean_price_gasoline_self

# 1. Im-Pesaran-Shin (IPS) test
# Allows for heterogeneous autoregressive coefficients across stations
ips_test <- purtest(price_var, test = "ips", exo = "intercept", lags = "AIC", pmax = 4)
summary(ips_test)

# 2. Levin-Lin-Chu (LLC) test
# Assumes a common autoregressive coefficient across all stations
llc_test <- purtest(price_var, test = "levinlin", exo = "intercept", lags = "AIC", pmax = 4)
summary(llc_test)

