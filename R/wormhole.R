#' Travel to a point in Time
#' 
#' @export
wormhole <- function(at,
                     timemachine.history = getOption("timemachine.history"),
                     timemachine.expose = getOption("timemachine.expose", c("ts", "data.frame")),
                     envir = globalenv(),
                     verbose = TRUE){

  dta <- timemachine.history %>% 
    group_by(var) %>% 
    filter(pub_date <= at) %>% 
    filter(pub_date == max(pub_date)) %>% 
    ungroup() %>% 
    select(-pub_date)


  if ("ts" %in% timemachine.expose){
    dta %>% 
      ts_ts %>% 
      tstools::mts.attach(envir = envir, warn = FALSE, na.omit = TRUE)

    if (verbose){
      message("time series attached")
    }
  }

  non.ts.expose <- setdiff(timemachine.expose, "ts")

  if (length(non.ts.expose) > 0){
    assign(".data", dta, envir = envir)
    if (verbose){
      message(".data attached")
    }
  }
}
