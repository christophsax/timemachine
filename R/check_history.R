#' @export
check_history <- function(history) {

  if (!inherits(history, "data.frame")) {
    stop("history must be of class data.frame")
  }

  ensure_col <- function(what) {
    if (!what %in% names(history)) {
      stop("column", what, "required in history but not present. See ?swiss_history for an example.'")
    }
  }
  lapply(c("id", "ref_date", "value"), ensure_col)

  key.name <- setdiff(colnames(history), c("id", "ref_date", "value"))
  if (length(key.name) > 1 && key.name != "id") {
    message("history: changing key name to 'id'.")
    history <- history %>%
      rename(id = !! key.name)
  }

  history <- select(history, pub_date, ref_date, value, everything())


  # ensure class.
  # What about fct id, fct or boolan value?
  ensure_class <- function(x, cl) {
    if (cl == "numeric") {
      if (!inherits(x, cl)) x <- as.numeric(x)
    } else if (cl == "numeric") {
      if (!inherits(x, cl)) x <- as.character(x)
    } else if (cl == "Date") {
      if (!inherits(x, cl)) x <- as.Date(x)
    }
    x
  }

  history <- history %>%
    mutate(
      pub_date = ensure_class(pub_date, "Date"),
      ref_date = ensure_class(ref_date, "Date")
    )

  if ("id" %in% colnames(history)) {
    history <- select(history, id, everything()) %>%
      mutate(
        id = ensure_class(id, "character")
      )
  }


  history
}

