### Preparing the Time Trip

library(timemachine)

# Constructing pseudo history data. It is assumed that mdeath is available
# one month after the end of the period, but fdeath immediately.
timemachine.history <- bind_rows(
  pseudo_history(ts_tbl(mdeaths), "1 month"),
  pseudo_history(ts_tbl(fdeaths))
)

# Currently, the id must be a single column called var, but multiple id columns
# with abitrary names are planned.

# Telling the time machine where to find the history
options(timemachine.history = timemachine.history)

# Telling the time machine where to evaluate
options(timemachine.dates = seq(as.Date("1978-01-01"), to = as.Date("1979-10-01"), by = "month"))

# Wormhole without an argument makes the latest data available in the
# globalenv(). This is useful to build the models.
wormhole()

# latest() returns the latest data, without writing to globalenv()

### Bon Voyage

# evalating some models from the forecast package
library(forecast)


# Put each model in a (named) expression. You can construct
dta <- timemachine(
  etf = {
    m <- forecast(mdeaths, h = 3)
    m$mean
  },
  arima = {
    m <- forecast(auto.arima(mdeaths), h = 3)
    m$mean
  },
  arimax = {
    m <- forecast(auto.arima(mdeaths,
                             xreg = window(fdeaths, end = end(mdeaths))),
                  xreg = window(fdeaths, start = tsp(mdeaths)[2] + 1/12),
                  h = 1)
    m$mean
  },
  randomwalk = {
    h = 3
    e <- tsp(mdeaths)[2]
    f <- tsp(mdeaths)[3]
    ts(mdeaths[length(mdeaths)], start = e + 1/f, end = e + h/f, f = f)
  }
)

### Back to the Future

errors <-
  dta %>%
  group_by(pub_date, expr) %>%
  mutate(h = seq(n())) %>%
  ungroup() %>%
  rename(fct = value) %>%
  mutate(var = "mdeaths") %>%
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


