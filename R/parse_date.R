#' Convert to date
#' 
#' Date converter with simpilar stucture
#' - Naming convention: `parse_date_[EXAMPLE]
#' - Apply operation on unique values only (use `apply_to_unique()`)
#' - Use `stringi`?
#' 
#' @param character vector
#' @examples
#' parse_date_2000colon1(c("2000:1", "2000:2"))
#' 
#' # These should drop an error:
#' # parse_date_2000colon1(c("2000:5"))
#' # parse_date_2000colon1(c("00:1"))
#' @export
parse_date_2000colon1 <- function(x){
  apply_to_unique(
    function(x){
      sp <- stringi::stri_split_fixed(x, ":", simplify = TRUE)
      stopifnot(ncol(sp) == 2L)
      year <- sp[, 1]
      qrt <- sp[, 2]

      check_subset(qrt, 1:4)
      check_subset(year, 1950:2080)
      as.Date(paste(year, quarter_to_month(qrt), 1, sep = "-"))
    }, 
    x
  )
}

check_subset <- function(x, of){
  sd <- setdiff(x, of)
  if (length(sd) > 0){
    stop("Values not in mapping: ", paste(sd, collapse = ", "))
  }
}

apply_to_unique <- function(fun, x){
  xu <- unique(x)
  zu <- fun(xu)
  left_join(data_frame(x = x), data_frame(x = xu, z = zu), by = "x")$z
}

quarter_to_month <- function(q){
  q <- as.numeric(q)
  stopifnot(q %in% 1:4)
  (q - 1) * 3 + 1
}
