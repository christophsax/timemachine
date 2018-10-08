library(testthat)

context("basic setup works as expected")


test_that("minimal pseudo history example works correctly", {

  history <-
    bind_rows(
      pseudo_history(mdeaths, "1 month"),
      pseudo_history(fdeaths),
    )  %>%
    check_history()

  # Telling the time machine where to evaluate
  dates = seq(as.Date("1979-04-01"), to = as.Date("1979-06-01"), by = "month")

  # Put each model in a (named) expression. You can construct
  fct_data <- timemachine(
    etf = {
      m <- forecast(mdeaths, h = 1)
      m$mean
    },
    arimax = {
      m <-
        forecast(
          auto.arima(mdeaths, xreg = window(fdeaths, end = end(mdeaths))),
          xreg = window(fdeaths, start = tsp(mdeaths)[2] + 1/12),
          h = 1
        )
      m$mean
    },
    history = history,
    dates = dates
  )

  # need to find a clever way to get 'reference data'
  bench_data <-
    latest(history) %>%
    ts_pick("mdeaths") %>%
    select(ref_date = time, ref_value = value)

  errors <- fct_data %>%
    inner_join(bench_data, by = "ref_date") %>%
    # add fct horizon
    group_by(pub_date, expr) %>%
    mutate(h = seq(n())) %>%
    ungroup()  %>%
    mutate(error = value - ref_value)

  # error stats
  ok <- errors %>%
    group_by(expr) %>%
    summarize(rmse = sqrt(sum(error^2)), mae = (mean(abs(error)))) %>%
    ungroup() %>%
    mutate(rmse.ok = rmse > c(77, 45) & rmse < c(79, 47)) %>%
    pull(rmse.ok)

  expect_true(all(ok))

})
