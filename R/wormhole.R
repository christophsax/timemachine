# timemachine.env <- new.env()

# if (".TimemachineEnv" %in% search()) detach(".TimemachineEnv")


# should be an environment in the serch path of globalenv(), but I don't know
# how to do that.

# Or maybe just leave it like that
timemachinenv <- function() {
  globalenv()
}


#' Travel to a Specific Point in Time
#'
#' Opening a wormhole enables time travel to a specific point in time. By
#' default, the `history` data frame is exposed to the global
#' enviroment as it was available on that day.
#'
#' @param date `"Date"` or `"character"`, the date where to a wormhole should be
#'   opened.
#'
#' @param history a data frame containing the history of the data.
#'   Must have column names `pub_date`, `ref_date` and `value`, plus a column
#'   that idenfies multiple series. Currently, this must be a single column
#'   `id`, but this will change. Usually, `history` is set as an
#'   option (see examples)
#'
#' @param as character, how to expose the variables in history. By default, the
#'   function exposes time series as single `"ts"` objects and a data frame,
#'   named `.data`, but all `ts_boxable` objects are supported.
#'
#' @param envir environment where to expose the data.
#'
#' @param verbose logical, what objects will be exposed?
#'
#' @export
#' @importFrom anytime anydate
#' @import tsbox
wormhole <- function(date = NULL,
                     history = getOption("timemachine.history"),
                     envir = globalenv(),
                     verbose = TRUE) {
  if (is.null(date)) date <- Sys.Date()
  TODAY <- anytime::anydate(date)

  if (verbose) {
    message(
      "Opening wormhole in ", TODAY,
      ". \nRefer to 'DATA' and 'TODAY' to use the available information at this point in time."
      )
  }

  check_history(history)

  # data available at date
  DATA <- history %>%
    filter(pub_date <= TODAY) %>%
    group_by(id) %>%
    filter(pub_date == max(pub_date)) %>%
    ungroup() %>%
    transmute(time = ref_date, value, id)

  if (!is.null(envir)){
    assign("DATA", DATA, envir = envir)
    assign("TODAY", TODAY, envir = envir)
  }

  return(invisible(DATA))
}

#' @export
latest <- function(...) {
  z <- wormhole(verbose = FALSE, envir = NULL)
  z
}
