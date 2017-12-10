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