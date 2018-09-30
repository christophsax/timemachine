#' Travel Through Time
#'
#' Core function of the package. Runs through a series of dates and evaluates
#' one or several expressions with the data available. Collects `ts_boxable`
#' objects returned by the expressions.
#'
#' @param ... expressions to be evaluated. Expressions can be named, so the name
#'   will appear in the output
#' @param timemachine.dates expressions to be evaluated. Usually specifyied via
#'   options. See examples. Expressions can be named, so the name
#'   will appear in the output
#'
#' @examples
#' library(tsbox)
#' library(timemachine)
#' library(dplyr)
#' library(tidyr)
#'
#' # Constructing pseudo history data. It is assumed that mdeath is available
#' # one month after the end of the period, but fdeath immediately.
#' ldeaths.history <- bind_rows(
#'   pseudo_history(ts_tbl(mdeaths), "1 month"),
#'   pseudo_history(ts_tbl(fdeaths))
#' )
#'
#' options(timemachine.history = ldeaths.history)
#' options(timemachine.dates = seq(as.Date("1979-10-01"), to = as.Date("1979-12-01"), by = "month"))
#'
#' library(forecast)
#' timemachine({
#'   m <- forecast(auto.arima(mdeaths))
#'   m$mean   # expression must evaluate to a tsboxable object
#' })
#'
#' # Using multiple experessions
#' timemachine(
#'   etf = {
#'     m <- forecast(mdeaths)
#'     m$mean
#'   },
#'   arima = {
#'     m <- forecast(auto.arima(mdeaths))
#'     m$mean
#'   }
#' )
#'
#' @export
timemachine <- function(...,
                        timemachine.dates = getOption("timemachine.dates")) {
  stopifnot(!is.null(timemachine.dates))
  timemachine.dates <- as.Date(timemachine.dates)

  exprs <- as.list(match.call(expand.dots = FALSE)$...)
  exprs.names <- names(exprs)
  if (is.null(exprs.names)) exprs.names <- paste0("ans", seq_along(exprs))
  exprs.names <- make.unique(exprs.names)

  env <- environment()
  ll <- list()
  for (i in seq_along(timemachine.dates)) {
    message(timemachine.dates[i])
    wormhole(timemachine.dates[i], envir = env, verbose = FALSE)

    anss <- lapply(exprs, function(e) try(eval(e, envir = env)))

    if (any(sapply(anss, inherits, "try-error"))) {
      wormhole(timemachine.dates[i])
      stop("Evaluation error. Opening wormhole at time of occurence.", call. = FALSE)
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
      mutate(pub_date = timemachine.dates[i])
  }

  bind_rows(ll) %>%
    rename(ref_date = time) %>%
    select(pub_date, ref_date, expr, value, everything())
}
