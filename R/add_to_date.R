#' Add Days, Weeks, Months, Quarters or Years to Dates
#' 
#' @param by can be specified in several ways.
#'     - A number, taken to be in days.
#'     - A object of class difftime
#'     - A character string, containing one of "day", "week", "month", "quarter" or "year". This can optionally be preceded by a (positive or negative) integer and a space, or followed by "s".
#' 
#' @export
add_to_date <- function(x, by = NULL){
  x <- as.Date(x)
  if (is.null(by)) return(x)
  add_to_one_date <- function(x) seq(x, length.out = 2, by = by)[2]
  apply_to_unique(
    function(x){
      do.call(c, lapply(x, add_to_one_date))
    }, 
    x
  )
}
