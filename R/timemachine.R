#' Travel Through Time
#' 
#' @examples
#' library(tsbox)
#' library(timemachine)
#' library(dplyr)
#' library(tidyr)
#' 
#' # Constructing a pseudo data history. It is assumed that mdeath is available 
#' # one month after the end of the period, but fdeath immediately. 
#' timemachine.history <- bind_rows(
#'   pseudo_history(ts_tbl(mdeaths), "1 month"),
#'   pseudo_history(ts_tbl(fdeaths))
#' )
#' 
#' options(timemachine.history = timemachine.history)
#' 
#' # options
#' options(timemachine.expose = c("ts", "data.frame"))
#' options(timemachine.expose = c("ts", "tbl"))
#' 
#' library(forecast)
#' dates <- seq(as.Date("1978-01-01"), to = as.Date("1979-12-01"), by = "month")
#' 
#' timemachine({
#'   m <- forecast(auto.arima(mdeaths))
#'   m$mean   # expression must evaluate to a tsboxable object
#' }, dates = dates)
#' 
#' @export
timemachine <- function(expr, dates){
  env <- environment()
  ll <- list()
  for (i in seq_along(dates)){
    message(".", appendLF = FALSE)
    wormhole(dates[i], envir = env, verbose = FALSE)
    ans <- eval(expr, envir = env)
    stopifnot(ts_boxable(ans))
    ll[[i]] <- ts_tbl(ans) %>% 
      mutate(pub_date = dates[i])
  }
  message("done!", appendLF = TRUE)
  bind_rows(ll)
}
