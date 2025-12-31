# Installing/loading packages (run once if needed)
packages <- c("tidyquant", "tidyverse", "lubridate", "scales", "janitor")
installed <- rownames(installed.packages())
for (p in packages) if (!p %in% installed) install.packages(p)
lapply(packages, library, character.only = TRUE)

# 1) Defining tickers (Magnificent Seven)
tickers <- c("AAPL","MSFT","AMZN","GOOGL","META","NVDA","TSLA")  # Apple, Microsoft, Amazon, Alphabet, Meta, Nvidia, Tesla

# 2) Setting date range
start_date <- as.Date("2020-01-01")
end_date   <- Sys.Date()

# 3) Fetching daily adjusted prices
prices <- tq_get(tickers, get = "stock.prices", from = start_date, to = end_date) %>%
  select(symbol, date, adjusted) %>%
  arrange(symbol, date)

# 4) Computing daily returns per ticker
returns <- prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", type = "log") %>%
  rename(ret = daily.returns) %>%
  ungroup()

# 5) Defining portfolio weights (example: equal-weight)
weights <- tibble(
  symbol  = tickers,
  weight  = c(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
)

# 6) Combining returns with weights and computing portfolio daily return
port_ret_daily <- returns %>%
  left_join(weights, by = "symbol") %>%
  mutate(weighted_ret = ret * weight) %>%
  group_by(date) %>%
  summarise(port_ret = sum(weighted_ret), .groups = "drop") %>%
  arrange(date)

# 7) Computing cumulative growth of $10,000
initial_value <- 10000
port_value <- port_ret_daily %>%
  mutate(growth_index = exp(cumsum(port_ret)),
         portfolio_value = initial_value * growth_index)

# 8) Creating monthly, quarterly aggregates
port_monthly <- port_ret_daily %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    port_ret_month = sum(port_ret),
    growth_index_month = exp(cumsum(port_ret_month)),
    .groups = "drop_last"
  ) %>%
  ungroup() %>%
  mutate(portfolio_value_month = initial_value * growth_index_month)

# 9) Risk metrics (rolling volatility, max drawdown)
port_risk <- port_ret_daily %>%
  mutate(
    # Rolling 60-day volatility (annualized)
    roll_vol_60d = sqrt(252) * zoo::rollapply(port_ret, 60, sd, fill = NA, align = "right")
  )

# Max drawdown from cumulative index
cum_index <- port_ret_daily %>%
  mutate(cum = exp(cumsum(port_ret)),
         peak = cummax(cum),
         drawdown = (cum / peak) - 1)

max_dd <- min(cum_index$drawdown, na.rm = TRUE)

# 10) Per-ticker contributions (monthly)
ticker_monthly_contrib <- returns %>%
  mutate(month = floor_date(date, "month")) %>%
  left_join(weights, by = "symbol") %>%
  group_by(symbol, month) %>%
  summarise(contrib = sum(ret * weight), .groups = "drop")

# 11) Per-ticker monthly returns (for heatmaps)
ticker_monthly_ret <- returns %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(symbol, month) %>%
  summarise(ret_month = sum(ret), .groups = "drop")

# 12) Price index per ticker for relative performance (rebased to 100 at start)
rebased <- prices %>%
  group_by(symbol) %>%
  mutate(index_100 = 100 * adjusted / first(adjusted)) %>%
  ungroup()

# 13) Saving CSVs for Tableau
write_csv(prices,                "prices.csv")
write_csv(returns,               "returns.csv")
write_csv(port_ret_daily,        "portfolio_daily_returns.csv")
write_csv(port_value,            "portfolio_value_daily.csv")
write_csv(port_monthly,          "portfolio_monthly.csv")
write_csv(port_risk,             "portfolio_risk_daily.csv")
write_csv(cum_index,             "portfolio_drawdown_daily.csv")
write_csv(ticker_monthly_contrib,"ticker_monthly_contrib.csv")
write_csv(ticker_monthly_ret,    "ticker_monthly_returns.csv")
write_csv(rebased,               "rebased_prices.csv")

# 14) Saving static weights table for reference
write_csv(weights,               "weights.csv")

# 15) Printing key summary stats
cat("Max drawdown (portfolio):", scales::percent(max_dd), "\n")
cat("Latest portfolio value:", scales::dollar(tail(port_value$portfolio_value, 1)), "\n")

