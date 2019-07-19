# FORECAST EVALUATION

library(timemachine)
library(forecast)
library(tidyverse)
library(tsbox)
library(bdfm)

# Constructing pseudo history data.

series_q <- c(
  'A191RL1Q225SBEA',       # 01 Real GDP, seasonally adjusted, quarterly, annualized % change
  'W068RCQ027SBEA'         # 02 Governemnt expenditures
)

series_m <- c(
  'USSLIND',               # 03 Federal Reserve leading index, monthly, percent
  'PCEDG',                 # 04 persional consumption: durable goods, monthly, level
  'PCEND',                 # 05 persional consumption: non-durable goods, monthly, level
  'UMCSENT',               # 06 Consumer Sentiment, monthly, delayed 1 month for free data
  'UNRATE',                # 07 Unemployment, monthly
  'JTSJOL',                # 08 Job openenings, total non-farm
  'INDPRO',                # 09 Industrial Production Index, monthly, level
  'CSUSHPINSA',            # 10 Case-Shiller home price index, monthly, two month lag, level
  'HSN1F',                 # 11 New 1 family houses sold, level
  'TSIFRGHT',              # 12 Freight transportation index, monthly, 2-3 month lag, level
  'FRGSHPUSM649NCIS',      # 13 CASS freight index, level, not SA
  'CAPUTLG2211S',          # 14 Electricity usage, % capacity, monthly
  'IPG2211S',              # 15 Electricity, industrial production index, monthly, level
  'DGORDER',               # 16 New Orders, durable manufacturing goods, monthly, level
  'AMTMNO',                # 17 New Orderes, all manufacuring industries, level
  'MNFCTRIRSA',            # 18 Manufacturers inventories:sales ratio
  'RETAILIRSA',            # 19 Retail inventories:sales ratio
  'WHLSLRIRSA',            # 20 Wholesalers, inventories:sales ratio
  'CPILFESL'               # 21 CPI
)

series_wd <- c(
  'ICSA',                  # 22 Initial claims, SA, weekly
  'TWEXB',                 # 23 exchange rate index, weekly
  'T10Y3M'                 # 24 10Y to 3M treasury spread, daily
)


econ_us <-
  ts_tbl(econ_us)

delays <- tribble(
  ~id,                 ~delay,
  "A191RL1Q225SBEA",   "3 month",
  "W068RCQ027SBEA",    "3 month",
  "USSLIND",           "1 month",
  "PCEDG",             "1 month",
  "PCEND",             "1 month",
  "UMCSENT",           "1 month",
  "UNRATE",            "1 month",
  "JTSJOL",            "1 month",
  "INDPRO",            "1 month",
  "CSUSHPINSA",        "1 month",
  "HSN1F",             "1 month",
  "TSIFRGHT",          "1 month",
  "FRGSHPUSM649NCIS",  "1 month",
  "CAPUTLG2211S",      "1 month",
  "IPG2211S",          "1 month",
  "DGORDER",           "1 month",
  "AMTMNO",            "1 month",
  "MNFCTRIRSA",        "1 month",
  "RETAILIRSA",        "1 month",
  "WHLSLRIRSA",        "1 month",
  "CPILFESL",          "1 month",
  "ICSA",              "1 month",
  "TWEXB",             "1 month",
  "T10Y3M",            "1 month"
)

history <- econ_us %>%
  nest(time, value) %>%
  left_join(delays, by = "id") %>%
  mutate(data = Map(pseudo_history, x = data, by = delay)) %>%
  unnest() %>%
  select(-delay, -id1) %>%
  filter(!is.na(value))

check_history(history)

# Telling the time machine where to evaluate
dates = seq(as.Date("2018-09-01"),
            to = as.Date("2018-10-01"),
            by = "month")

# wormhole(history, "2012-02-01")

results <- timemachine(
  bdfm = {
    m <- dfm(DATA, forecast = 6, pre_differenced = 'A191RL1Q225SBEA', keep_posterior = 'A191RL1Q225SBEA')

    # this should be done by dfm()
    valid.time <-
      ts_pick(predict(m), "A191RL1Q225SBEA") %>%
      ts_na_omit() %>%
      pull(time)

    m$Ymedian %>%
      filter(time %in% valid.time) %>%
      ts_ts()

  },
  etf = {
    m <- forecast(A191RL1Q225SBEA, h = 3)
    m$mean
  },
  aa = {
    m <- forecast(auto.arima(A191RL1Q225SBEA), h = 3)
    m$mean
  },
  history = history,
  dates = dates
)

data.table::fwrite(results, "timemachine_results.csv")

