#' rv_create_airplane UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rv_create_airplane_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::knobInput(
      inputId = NS(id, "risk_01_ratio"),
      label = "Risk 1",
      value = 20,
      min = 0,
      max = 100,
      step = 1,
      post = "%"
    ),
    shinyWidgets::knobInput(
      inputId = NS(id, "risk_02_ratio"),
      label = "Risk 2",
      value = 5,
      min = 0,
      max = 100,
      step = 1,
      post = "%"
    ),
    shinyWidgets::knobInput(
      inputId = NS(id, "risk_03_ratio"),
      label = "Risk 3",
      value = 0,
      min = 0,
      max = 100,
      step = 1,
      post = "%"
    )
  )
}

#' rv_create_airplane Server Functions
#'
#' @noRd
mod_rv_create_airplane_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    renderPlot(
      {
        tibble::tribble(
          ~Risk, ~Ratio,
          "Risk 1", input$risk_01_ratio / 100,
          "Risk 2", input$risk_02_ratio / 100,
          "Risk 3", input$risk_03_ratio / 100
        ) %>%
          rv_create_airplane()
      },
      res = 96,
      height = 1024
    )
  })
}

## To be copied in the UI
# mod_rv_create_airplane_ui("rv_create_airplane_1")

## To be copied in the server
# mod_rv_create_airplane_server("rv_create_airplane_1")


rvs_create_airplane <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        mod_rv_create_airplane_ui("rv_create_airplane_1")
      ),
      mainPanel = mainPanel(
        plotOutput("airplane_gg")
      )
    )
  )
  server <- function(input, output, session) {
    output$airplane_gg <- mod_rv_create_airplane_server("rv_create_airplane_1")
  }
  shinyApp(ui, server)
}
