#' 'Attach' single `ts` objects
#'
#' Assigns time series from tsboxable objects as single `ts` objects, usually
#' to the global environment.
#'
#' @param x a tsboxable time series object.
#' @param envir an environment, where to assign the  `ts` objects.
#' @export
ts_attach <- function(x, envir = globalenv()) {
  stopifnot(ts_boxable(x))
  tl <- ts_tslist(x)
  message("Attached: ", paste(names(tl), collapse = ", "))
  Map(
    function(...) assign(..., envir = envir),
    x = names(tl),
    value = tl
  )
  return(invisible(x))
}
