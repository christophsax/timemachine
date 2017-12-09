#' Add Days, Weeks, Months, Quarters or Years to Dates
#' 
#' @param by can be specified in several ways.
#'     - A number, taken to be in days.
#'     - A object of class difftime
#'     - A character string, containing one of "day", "week", "month", "quarter" or "year". This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s".
#' 
#' @export
add_to_date <- function(x, by){
  x <- as.Date(x)
  if (is.null(by)) return(x)
  add_to_one_date <- function(x) seq(x, length.out = 2, by = by)[2]
  do.call(c, lapply(x, add_to_one_date))
}

#' Pseudo History Data Frame
#' 
#' @examples
#' pseudo_history(ts_tbl(mdeaths), "1 month")
#' pseudo_history(ts_tbl(fdeaths))
#' @export
pseudo_history <- function(x, by = NULL){
  stopifnot(tsbox::ts_boxable(x))

  dta0 <- ts_tbl(x) %>% 
    rename(ref_date = time) %>% 
    mutate(pub_date = add_to_date(ref_date, by)) %>% 
    select(ref_date, pub_date, value, var)

  blow_up <- function(this){
    this %>% 
    select(.pub_date = pub_date) %>% 
    rowwise() %>% 
    mutate(data = list(filter(this, pub_date <= .pub_date))) %>% 
    ungroup() %>% 
    unnest() %>% 
    mutate(pub_date = .pub_date) %>% 
    select(-.pub_date)
  }

  dta0 %>% 
    split(dta0$var) %>% 
    lapply(blow_up) %>% 
    bind_rows() %>% 
    filter(!is.na(value))
}