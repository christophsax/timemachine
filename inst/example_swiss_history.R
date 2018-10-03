### Preparing the Time Trip

library(timemachine)

# assuming GPD.US is available one period before
# (for demonstration only)
swiss_history2 <- swiss_history %>%
  filter(id %in% c("EXP", "GDP.CH")) %>%
  mutate(pub_date = if_else(
    id == "EXP",
    add_to_date(pub_date, "-1 quarter"),
    pub_date
    )) %>%
  # pc rates
  group_by(id, pub_date) %>%
  mutate(value = log(value) - lag(log(value))) %>%
  filter(!is.na(value))

# Telling the time machine where to find the history
options(timemachine.history = swiss_history2)

# Telling the time machine where to evaluate
options(timemachine.dates = seq(as.Date("2014-01-01"), to = as.Date("2015-10-01"), by = "quarter"))

# Wormhole without an argument makes the latest data available in the
# globalenv(). This is useful to build the models.
wormhole()




# At a point in history, note that EXP is available but GDP.CH is not
wormhole("2014-07-01")


ts_attach(DATA)  # this is part of timeamchine, but could be elsewhere

GDP.CH <- ts_ts(ts_pick(DATA, "GDP.CH"))
EXP <- ts_ts(ts_pick(DATA, "EXP"))

# latest() returns the latest data, without writing to globalenv()

### Bon Voyage

# evalating some models from the forecast package
library(forecast)


# Put each model in a (named) expression. You can construct
dta <- timemachine(
  etf = {
    ts_attach(DATA)
    m <- forecast(GDP.CH, h = 3)
    m$mean
  },
  arima = {
    ts_attach(DATA)
    m <- forecast(auto.arima(GDP.CH), h = 3)
    m$mean
  },
  arima_exp = {
    ts_attach(DATA)
    m <- forecast(auto.arima(GDP.CH,
                             xreg = window(EXP, end = end(GDP.CH))),
                  xreg = window(EXP, start = tsp(GDP.CH)[2] + 1/12),
                  h = 1)
    m$mean
  },
  randomwalk = {
    ts_attach(DATA)
    h = 3
    e <- tsp(GDP.CH)[2]
    f <- tsp(GDP.CH)[3]
    ts(GDP.CH[length(GDP.CH)], start = e + 1/f, end = e + h/f, f = f)
  }
) %>%
  arrange(expr, pub_date, ref_date)

### Back to the Future


errors <-
  dta %>%
  group_by(pub_date, expr) %>%
  mutate(h = seq(n())) %>%
  ungroup() %>%
  rename(fct = value) %>%
  mutate(var = "GDP.CH") %>%
  left_join(rename(latest(), act = value)) %>%
  filter(!is.na(act)) %>%
  mutate(error = fct - act)

# error stats
errors %>%
  group_by(expr, h) %>%
  summarize(rmse = sqrt(sum(error^2)), mae = (mean(abs(error))))


# scatter plots, by horizon
library(ggplot2)
errors %>%
  ggplot() +
  geom_point(aes(x = fct, y = act)) +
  facet_grid(h ~ expr)


