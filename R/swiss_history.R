#' Real Time Data For Switzerland
#'
#' @docType data
#'
#' @format data frame
#'
#' @source SECO
#'
#' @name swiss_history
#' @keywords datasets
#' @examples
#'
#' ## import latest data
#' url <- "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/VIP%20Quartalssch%C3%A4tzungen/realtime_database.xls.download.xls/realtime_database.xls"
#'
#' library(readxl)
#' library(dplyr)
#' library(tidyr)
#' library(tsbox)
#' library(timemachine)
#'
#' tf <- tempfile(fileext = ".xls")
#' download.file(url, tf)
#'
#'
#' sheets <- readxl::excel_sheets(tf)
#'
#' swiss_history <-
#'   data_frame(var = setdiff(sheets, "title")) %>%
#'   rowwise() %>%
#'   mutate(data = list(read_excel(path = tf, sheet = var, skip = 10))) %>%
#'   ungroup() %>%
#'   unnest() %>%
#'   rename(ref_date = X__1) %>%
#'   gather(pub_date, value, -var, -ref_date) %>%
#'   mutate_at(c("ref_date", "pub_date"), parse_date_2000colon1) %>%
#'   select(var, pub_date, ref_date, value) %>%
#'   filter(!is.na(value))
#'
#' # save(swiss_history, file = "data/swiss_history.RData")
#'
#' data(swiss_history)
#' swiss_history
NULL

