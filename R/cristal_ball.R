#' GUI for timemachine history data
#'
#' @param history a data frame with the publication history of the data.
#'  Must have column names `pub_date`, `ref_date` and `value`, plus a column
#'  that identifies multiple series. Use `check_history()` to check the validity
#'  of a history data.frame
#'
#' @export
#' @import shiny
#' @import miniUI
#' @import dygraphs
#' @importFrom clipr write_clip
#' @examples
#' \dontrun{
#' cristal_ball(swiss_history)
#' }
cristal_ball <- function(history){

  history.name <- deparse(substitute(history))

  history <- check_history(history)
  pub_date_unique <- unique(history$pub_date)
  id_unique <- unique(history$id)

  # http://shiny.rstudio.com/articles/gadget-ui.html
  ui <- miniPage(
    gadgetTitleBar("Cristal Ball"),
    miniContentPanel(
      selectInput(
        "iId", label = NULL,
        choices = id_unique, selected = id_unique[1],
        multiple = TRUE,
        width = '100%'
      ),
      dygraphOutput("oGraph"),
      sliderInput("iPub_date", "Publication time",
        min = min(pub_date_unique),
        max = max(pub_date_unique),
        value = max(pub_date_unique),
        animate = animationOptions(
          interval = 2,
          loop = TRUE
        ),
        width = '100%',
      )

    )

  )

  server <- function(input, output) {

    rSeries <- reactive({
      .id <- input$iId
      filter(history, .data$id %in% .id)
    })

    output$oGraph <- renderDygraph({
      latest.series <- rSeries() %>%
        filter(.data$pub_date <= input$iPub_date) %>%
        filter(.data$pub_date == max(.data$pub_date)) %>%
        select(-.data$pub_date, time = .data$ref_date)
      validate(need(nrow(latest.series) > 0, "no data"))
      dygraph(ts_xts(latest.series))
    })

    observe({
      if (input$done > 0){
        x <- input$iPub_date
        message("Copied to clipboard: \n")
        cstr <- paste0('wormhole(', history.name, ', "', x, '")')
        clipr::write_clip(cstr)
        message(cstr)
        stopApp(returnValue = invisible(cstr))
      }
    })

    observe({
      if (input$cancel > 0){
        stopApp(returnValue = invisible())
      }
    })

  }
  runGadget(shinyApp(ui = ui, server = server), stopOnCancel = FALSE)
}
