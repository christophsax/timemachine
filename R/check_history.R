#' @export
check_history <- function(timemachine.history) {

  if (!inherits(timemachine.history, "data.frame")) {
    stop("history must be of class data.frame")
  }

  ensure_col <- function(what) {
    if (!what %in% names(timemachine.history)) {
      stop("column", what, "required in history but not present. See ?swiss_history for an example.'")
    }
  }
  lapply(c("pub_date", "ref_date", "value"), ensure_col)
  return(invisible(TRUE))
}

