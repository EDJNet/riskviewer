#' rv_create_airplane UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_rv_create_airplane_compact_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::sliderInput(inputId =  NS(id, "airplanes_number"),
                       round = 1,
                        label = "Number of airplanes",
                        value = 2,
                        min = 1,
                        max = 10,
                        step = 1),
    shinyWidgets::knobInput(inputId = NS(id, "risk_01_ratio"),
                            label = "Risk 1",
                            value = 20,
                            min = 0,
                            max = 100,
                            step = 1,
                            post = "%",
                            fgColor = "#ff7256"),
    
    shinyWidgets::knobInput(inputId = NS(id, "risk_02_ratio"),
                            label = "Risk 2",
                            value = 5,
                            min = 0,
                            max = 100,
                            step = 1,
                            post = "%",
                            fgColor = "#008b8b"),
    
    shinyWidgets::knobInput(inputId = NS(id, "risk_03_ratio"),
                            label = "Risk 3",
                            value = 0,
                            min = 0,
                            max = 100,
                            step = 1,
                            post = "%",
                            fgColor = "#ffb90f")
  )
}

#' rv_create_airplane Server Functions
#'
#' @noRd 
mod_rv_create_airplane_compact_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    renderPlot({
      tibble::tribble(~Risk, ~Ratio,
                      "Risk 1", input$risk_01_ratio/100,
                      "Risk 2", input$risk_02_ratio/100,
                      "Risk 3", input$risk_03_ratio/100) %>% 
      rv_create_airplane_combo(number_of_planes = as.numeric(input$airplanes_number),
                               compact = TRUE, nrow = 2, ncol = 5)
    }, res = 96, height = 1024)  %>% 
      shiny::bindCache(list(input$risk_01_ratio, input$risk_02_ratio, input$risk_03_ratio, input$airplanes_number),
                       sizePolicy = sizeGrowthRatio(width = 1024, height = 1024, growthRate = 1))
  })
}

## To be copied in the UI
# mod_rv_create_airplane_compact_ui("rv_create_compact_airplane_1")

## To be copied in the server
# mod_rv_create_airplane_compact_server("rv_create_compact_airplane_1")


rvs_create_airplane_compact <- function() {
  ui <- fluidPage(
   sidebarLayout(sidebarPanel = sidebarPanel(
      mod_rv_create_airplane_compact_ui("rv_create_compact_airplane_1"),
      width = 2),
    mainPanel = mainPanel(
      plotOutput(outputId = "airplane_gg", height = 1024)
    )
    )
    
  )
  server <- function(input, output, session) {
    output$airplane_gg <- mod_rv_create_airplane_compact_server("rv_create_compact_airplane_1")
  }
  shinyApp(ui, server)  
}
