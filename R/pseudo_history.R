#' Pseudo History Data Frame
#'
#' If no historical data is available, *pseudo history* data can be produced
#' from current data. The function replicates the data for each observational
#' point. If `by` is specified, a publication delay will be assumed.
#'
#' `by` can be specified in several ways.
#'   - A number, taken to be in days.
#'   - A object of class `difftime`
#'   - A character string, containing one of "day", "week", "month", "quarter" or "year". This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s".
#'
#' @param x a `ts_boxable` time series.
#' @param by offset of publication date. See details.
#'
#' @examples
#' library(tsbox)
#' pseudo_history(ts_c(mdeaths, fdeaths), "1 month")
#' pseudo_history(fdeaths)
#' @export
pseudo_history <- function(x, by = NULL) {
  stopifnot(ts_boxable(x))

  dta0 <- ts_tbl(x) %>%
    rename(ref_date = .data$time) %>%
    mutate(pub_date = add_to_date(.data$ref_date, by)) %>%
    select(starts_with("id"), .data$pub_date, .data$ref_date, .data$value)

  pub_date <- NULL
  .pub_date <- NULL

  blow_up <- function(this) {
    this %>%
      select(.pub_date = pub_date) %>%
      rowwise() %>%
      mutate(data = list(filter(this, pub_date <= .pub_date))) %>%
      ungroup() %>%
      unnest() %>%
      select(-pub_date) %>%
      rename(pub_date = .pub_date)
  }

  if (ncol(dta0) > 3) {
    z <- dta0 %>%
      split(dta0$id) %>%
      lapply(blow_up) %>%
      bind_rows() %>%
      filter(!is.na(.data$value)) %>%
      select(starts_with("id"), .data$pub_date, .data$ref_date, .data$value)
  } else {
    z <- dta0 %>%
      blow_up() %>%
      filter(!is.na(.data$value))

    # add id col for single series
    .id <- deparse(substitute(x))
    z <-
      mutate(z, id = .id) %>%
      select(.data$id, everything())

  }

  z
}
