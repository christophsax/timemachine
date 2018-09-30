#' Travel to a Specific Point in Time
#'
#' Opening a wormhole enables time travel to a specific point in time. By
#' default, the `timemachine.history` data frame is exposed to the global
#' enviroment as it was available on that day.
#'
#' @param date `"Date"` or `"character"`, the date where to a wormhole should be
#'   opened.

#' @param timemachine.history a data frame containing the history of the data.
#'   Must have column names `pub_date`, `ref_date` and `value`, plus a column
#'   that idenfies multiple series. Currently, this must be a single column
#'   `var`, but this will change. Usually, `timemachine.history` is set as an
#'   option (see examples)
#'
#' @param timemachine.expose character, by default, the function exposes time
#'   series as single `"ts"` objects and a data frame, named `.data`, but all
#'   `ts_boxable` objects will be supported. Not yet implemented.
#'
#' @param envir environment where to expose the data.
#'
#' @param verbose logical, what objects will be exposed?
#'
#' @export
wormhole <- function(date = NULL,
                     timemachine.history = getOption("timemachine.history"),
                     timemachine.expose = getOption("timemachine.expose", c("ts", "data.frame")),
                     envir = globalenv(),
                     verbose = TRUE){

  if (is.null(date)) date <- Sys.Date()
  date <- as.Date(date)

  dta <- timemachine.history %>%
    filter(pub_date <= date) %>%
    group_by(var) %>%
    filter(pub_date == max(pub_date)) %>%
    ungroup() %>%
    transmute(time = ref_date, value, var)

  newobj <- NULL
  if ("ts" %in% timemachine.expose){
    ll <- split(dta, dta$var)
    Map(function(x, value) assign(x, ts_ts(value), envir = envir),
        x = names(ll), value = ll)
    newobj <- unique(timemachine.history$var)
  }

  non.ts.expose <- setdiff(timemachine.expose, "ts")
  if (length(non.ts.expose) > 0){
    assign(".data", dta, envir = envir)
    newobj <- c(newobj, ".data")
  }
  newobj <- c(newobj, ".today")
  assign(".today", date, envir = envir)
  if (verbose){
    message("Opening wormhole on the ", date, " for the following objects:")
    message(paste(newobj, collapse = ", "))
  }

  return(invisible(dta))
}

#' @export
latest <- function(...){
  z <- wormhole(verbose = FALSE, timemachine.expose = NULL)
  z
}





