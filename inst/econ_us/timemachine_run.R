# FORECAST EVALUATION

library(timemachine)
library(forecast)
library(tidyverse)
library(tsbox)
library(bdfm)

# Constructing pseudo history data. See ?econ_us for var definition
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
  ts_tbl() %>%
  nest(time, value) %>%
  left_join(delays, by = "id") %>%
  mutate(data = Map(pseudo_history, x = data, by = delay)) %>%
  unnest(cols = c(data)) %>%
  select(-delay, -id1) %>%
  filter(!is.na(value))

check_history(history)

# Telling the time machine where to evaluate
dates = seq(as.Date("2008-01-01"),
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

