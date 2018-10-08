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
#'  opened.
#'
#' @param history a data frame with the publication history of the data.
#'  Must have column names `pub_date`, `ref_date` and `value`, plus a column
#'  that idenfies multiple series. Use `check_history()` to check the validity
#'  of a history data.frame
#'
#' @param post_process a function that is applied on `DATA`. By default,
#'  `ts_attach` adds makes series available as single `ts` objects. Alternatives
#'  are: `ts_ts` (to convert it to a multiple `mts` object), or any converter
#'  from tsbox.
#'
#' @param envir environment where to expose the data.
#'
#' @param verbose logical, what objects will be exposed?
#'
#' @export
wormhole <- function(history,
                     date = NULL,
                     envir = globalenv(),
                     verbose = TRUE,
                     post_process = ts_attach) {

  history <- check_history(history)

  if (is.null(date)) date <- Sys.Date()  # or we may open GUI

  TODAY <- anytime::anydate(date)
  if (verbose) {
    message(
      "Opening wormhole in ", TODAY,
      ". \nRefer to 'DATA' and 'TODAY' to use the available information at this point in time."
      )
  }


  # data available at date
  DATA <- history %>%
    filter(.data$pub_date <= TODAY) %>%
    group_by(id) %>%
    filter(.data$pub_date == max(.data$pub_date)) %>%
    ungroup() %>%
    transmute(time = .data$ref_date, .data$value, .data$id)

  if (!is.null(envir)) {
    assign("DATA", post_process(DATA), envir = envir)
    assign("TODAY", TODAY, envir = envir)
  }

  return(invisible(DATA))
}

#' Latest available daata
#' @param history a data.frame, containing historic data for one or several
#'   variables
#' @export
latest <- function(history) {
  z <- wormhole(
    history = history,
    post_process = function(x) x,
    envir = NULL,
    verbose = FALSE
  )
  z
}
