#' @export
ts_attach <- function(x, envir = globalenv()) {
  stopifnot(ts_boxable(x))
  tl <- ts_tslist(x)
  message("Attached: ", paste(names(tl), collapse = ", "))
  Map(
    function(...) assign(..., envir = globalenv()),
    x = names(tl),
    value = tl
  )
  return(invisible(TRUE))
}
