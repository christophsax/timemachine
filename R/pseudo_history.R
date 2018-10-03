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
#' See seq.POSIXt for the details of "month".
#'
#' @param x a `ts_boxable` time series.
#' @param by offset of publication date. See details.
#'
#' @examples
#' pseudo_history(ts_c(mdeaths, fdeaths), "1 month")
#' pseudo_history(fdeaths)
#' @export
pseudo_history <- function(x, by = NULL) {
  stopifnot(tsbox::ts_boxable(x))

  dta0 <- ts_tbl(x) %>%
    rename(ref_date = time) %>%
    mutate(pub_date = add_to_date(ref_date, by)) %>%
    select(starts_with("id"), pub_date, ref_date, value)

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
      filter(!is.na(value)) %>%
      select(starts_with("id"), pub_date, ref_date, value)
  } else {
    z <- dta0 %>%
      blow_up() %>%
      filter(!is.na(value))
  }

  z
}
