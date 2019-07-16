#' Travel Through Time
#'
#' Core function of the package. Runs through a series of dates and evaluates
#' one or several expressions with the data available.
#'
#' @param ... expressions to be evaluated. Expressions can be named, so the name
#'   will appear in the output
#' @param history a data frame with the publication history of the data.
#'  Must have column names `pub_date`, `ref_date` and `value`, plus a column
#'  that identifies multiple series. Use `check_history()` to check the validity
#'  of a history data.frame
#' @param dates Date or character. At which points in time should the
#'   expressions be evaluated?
#'
#' @examples
#' library(tsbox)
#' library(timemachine)
#' library(dplyr)
#' library(tidyr)
#'
#' # Pseudo History example ----------------------------------------------------
#'
#' # Constructing pseudo history data. It is assumed that mdeath is available
#' # one month after the end of the period, but fdeath immediately.
#' history <-
#'   bind_rows(
#'     pseudo_history(mdeaths, "1 month"),
#'     pseudo_history(fdeaths),
#'   )  %>%
#'   check_history()
#'
#' # Telling the time machine where to evaluate
#' dates = seq(as.Date("1978-01-01"), to = as.Date("1979-10-01"), by = "month")
#'
#' # Wormhole without an argument makes the latest data available in the
#' # globalenv(). This is useful to build the models.
#'
#' # Since version 0.0.2, 'history' is not set via options anymore, and must be
#' # provided all the time.
#'
#' wormhole(history)
#'
#' # latest() returns the latest data
#'
#' latest(history)
#'
#' ### Bon Voyage
#'
#' # evalating some models from the forecast package
#' library(forecast)
#'
#' # Put each model in a (named) expression. You can construct
#' fct_data <- timemachine(
#'   etf = {
#'     m <- forecast(mdeaths, h = 3)
#'     m$mean
#'   },
#'   arima = {
#'     m <- forecast(auto.arima(mdeaths), h = 3)
#'     m$mean
#'   },
#'   arimax = {
#'     m <-
#'       forecast(
#'         auto.arima(mdeaths, xreg = window(fdeaths, end = end(mdeaths))),
#'         xreg = window(fdeaths, start = tsp(mdeaths)[2] + 1/12),
#'         h = 1
#'       )
#'     m$mean
#'   },
#'   randomwalk = {
#'     h = 3
#'     e <- tsp(mdeaths)[2]
#'     f <- tsp(mdeaths)[3]
#'     ts(mdeaths[length(mdeaths)], start = e + 1/f, end = e + h/f, f = f)
#'   },
#'   history = history,
#'   dates = dates
#' )
#'
#'
#' # need to find a clever way to get 'reference data'
#' bench_data <-
#'   latest(history) %>%
#'   ts_pick("mdeaths") %>%
#'   select(ref_date = time, ref_value = value)
#'
#' errors <- fct_data %>%
#'   left_join(bench_data, by = "ref_date") %>%
#'   # add fct horizon
#'   group_by(pub_date, expr) %>%
#'   mutate(h = seq(n())) %>%
#'   ungroup()  %>%
#'   mutate(error = value - ref_value)
#'
#' # error stats
#' errors %>%
#'   group_by(expr, h) %>%
#'   summarize(rmse = sqrt(sum(error^2)), mae = (mean(abs(error))))
#'
#' # scatter plots, by horizon
#' library(ggplot2)
#' errors %>%
#'   ggplot() +
#'   geom_point(aes(x = value, y = ref_value)) +
#'   facet_grid(h ~ expr)
#'
#'
#' # Real time data ------------------------------------------------------------
#'
#' # Set up history
#' # assuming EXP is available one period before
#' swiss_history2 <- swiss_history %>%
#'   filter(id %in% c("EXP", "GDP.CH")) %>%
#'   mutate(pub_date = if_else(
#'     id == "EXP",
#'     add_to_date(pub_date, "-1 quarter"),
#'     pub_date
#'     )) %>%
#'   # pc rates
#'   group_by(id, pub_date) %>%
#'   mutate(value = log(value) - lag(log(value))) %>%
#'   ungroup() %>%
#'   filter(!is.na(value)) %>%
#'   check_history()
#'
#' # Simulation
#' # evalating some models from the forecast package
#' library(forecast)
#'
#' # Put each model in a (named) expression. You can construct
#' fct_data <- timemachine(
#'   etf = {
#'     m <- forecast(GDP.CH, h = 3)
#'     m$mean
#'   },
#'   arima = {
#'     m <- forecast(auto.arima(GDP.CH), h = 3)
#'     m$mean
#'   },
#'   arima_exp = {
#'     m <- forecast(auto.arima(GDP.CH,
#'                              xreg = window(EXP, end = end(GDP.CH))),
#'                   xreg = window(EXP, start = tsp(GDP.CH)[2] + 1/12),
#'                   h = 1)
#'     m$mean
#'   },
#'   randomwalk = {
#'     h = 3
#'     e <- tsp(GDP.CH)[2]
#'     f <- tsp(GDP.CH)[3]
#'     ts(GDP.CH[length(GDP.CH)], start = e + 1/f, end = e + h/f, f = f)
#'   },
#'   history = swiss_history2,
#'   dates = seq(as.Date("2014-01-01"), to = as.Date("2015-10-01"), by = "quarter")
#' )
#'
#' # Evaluation
#' # see example_annual_gdp.R for advanced use of benchmark data
#' bench_data <-
#'   latest(swiss_history2) %>%
#'   ts_pick("GDP.CH") %>%
#'   select(ref_date = time, ref_value = value)
#'
#' errors <- fct_data %>%
#'   left_join(bench_data, by = "ref_date") %>%
#'
#'   # add fct horizon
#'   group_by(pub_date, expr) %>%
#'   mutate(h = seq(n())) %>%
#'   ungroup()  %>%
#'   mutate(error = value - ref_value)
#'
#' # error stats
#' errors %>%
#'   group_by(expr, h) %>%
#'   summarize(rmse = sqrt(sum(error^2)), mae = (mean(abs(error))))
#'
#' # scatter plots, by horizon
#' library(ggplot2)
#' errors %>%
#'   ggplot() +
#'   geom_point(aes(x = ref_value, y = value)) +
#'   facet_grid(h ~ expr)
#'
#'
#' # Advanced benchmarking -----------------------------------------------------
#' # annual gdp growth
#' history_gdp <-
#'   swiss_history %>%
#'   filter(id == "GDP.CH") %>%
#'   ts_frequency("year", sum) %>%
#'   ts_pc()
#'
#' # 1st BFS value is available in Oct
#' bench_bfs <-
#'   history_gdp %>%
#'   filter(as.POSIXlt(pub_date)$mon + 1 == 7) %>%
#'   arrange(pub_date) %>%
#'   group_by(ref_date) %>%
#'   slice(1) %>%
#'   ungroup() %>%
#'   select(ref_date, ref_value = value)
#'
#' # 1st SECO value is the first available value
#' bench_seco <-
#'   history_gdp %>%
#'   arrange(pub_date) %>%
#'   group_by(ref_date) %>%
#'   slice(1) %>%
#'   ungroup() %>%
#'   select(ref_date, ref_value = value)
#'
#' @export
#' @importFrom stats time
#' @importFrom rlang .data
#' @importFrom tidyr unnest
#' @importFrom tsbox ts_boxable ts_c ts_tbl ts_tslist ts_xts
#' @importFrom anytime anydate
#' @import dplyr
timemachine <- function(...,dates, history, post_process = ts_attach) {

  history <- check_history(history)

  stopifnot(!is.null(dates))
  dates <- as.Date(dates)

  exprs <- as.list(match.call(expand.dots = FALSE)$...)
  exprs.names <- names(exprs)
  if (is.null(exprs.names)) exprs.names <- paste0("ans", seq_along(exprs))
  exprs.names <- make.unique(exprs.names)

  env <- environment()
  ll <- list()
  for (i in seq_along(dates)) {
    message(dates[i])
    wormhole(dates[i], history = history, envir = env, verbose = FALSE, post_process = post_process)

    anss <- lapply(exprs, function(e) try(eval(e, envir = env)))
    fails <- sapply(anss, inherits, "try-error")

    if (any(fails)) {
      wormhole(dates[i], history = history, post_process = post_process)
      stop("Evaluation error in: ",
        paste(names(anss)[fails], collapse = ", "),
        "Opening wormhole at time of occurence.",
        call. = FALSE
      )
    }

    is.boxable <- vapply(anss, ts_boxable, TRUE)
    if (!all(is.boxable)) {
      stop(
        "some expressions do not evaluate to a boxable object: ",
        paste(exprs.names[!is.boxable], collapse = ", ")
      )
    }
    anss.tbl <- lapply(anss, ts_tbl)
    names(anss.tbl) <- exprs.names

    ll[[i]] <- bind_rows(anss.tbl, .id = "expr") %>%
      mutate(pub_date = dates[i])
  }

  bind_rows(ll) %>%
    rename(ref_date = time) %>%
    select(expr, .data$pub_date, .data$ref_date, .data$value, everything()) %>%
    arrange(expr, .data$pub_date, .data$ref_date)
}
