#' Travel to a point in Time
#' 
#' @export
wormhole <- function(date = NULL,
                     timemachine.history = getOption("timemachine.history"),
                     timemachine.expose = getOption("timemachine.expose", c("ts", "data.frame")),
                     envir = globalenv(),
                     verbose = TRUE){

  if (is.null(date)) date <- Sys.Date()
  date <- as.Date(date)

  dta <- timemachine.history %>% 
    filter(pub_date <= date) %>% 
    group_by(var) %>% 
    filter(pub_date == max(pub_date)) %>% 
    ungroup() %>% 
    select(-pub_date)

  if (verbose){
    message("Travelling to ", date, "\n")
  }

  if ("ts" %in% timemachine.expose){
    ll <- split(dta, dta$var)
    Map(function(x, value) assign(x, ts_ts(value), envir = envir), 
        x = names(ll), value = ll)

    if (verbose){
      message("time series objects:\n", 
              paste(unique(timemachine.history$var), collapse = ", ")
              )
    }
  }

  non.ts.expose <- setdiff(timemachine.expose, "ts")

  if (length(non.ts.expose) > 0){
    assign(".data", dta, envir = envir)
    if (verbose){
      message("data.frame with time series:\n.data")
    }
  }

  return(invisible(dta))
}

#' @export
latest <- function(...){
  z <- wormhole(verbose = FALSE, timemachine.expose = NULL)
  z
}





