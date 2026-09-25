# ------------------------------------------------------------------------------
# 1. FETCH DAILY BRENT CRUDE OIL DATA (2016-2026)
# ------------------------------------------------------------------------------
library(quantmod)
library(dplyr)
library(zoo)

# Fetch Brent daily prices from FRED (Federal Reserve Bank of St. Louis)
getSymbols("DCOILBRENTEU", src = "FRED", from = "2016-01-01", to = "2026-12-31")

library(tidyr)

brent_df <- data.frame(
  date = ymd(index(DCOILBRENTEU)),
  brent_price = as.numeric(DCOILBRENTEU$DCOILBRENTEU)
) %>%
  # 1. Generate a continuous daily sequence to create rows for missing days
  complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  # 2. Last Observation Carried Forward (imputes from previous day)
  mutate(brent_price = na.locf(brent_price, na.rm = FALSE))

