#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
rv_explorer_app_ui <- function(request) {
  tagList(
    
    golem_add_external_resources(),
    
    fluidPage(shiny::titlePanel(title = tags$a("Riskviewer explorer - A tool by EDJNet", href='https://github.com/EDJNet/riskviewer'),
                                          windowTitle = "Riskviewer explorer - A tool by EDJNet"),
      shiny::sidebarLayout(sidebarPanel = sidebarPanel(
        mod_rv_create_airplane_compact_ui("rv_create_compact_airplane_1"),
        shiny::hr(),
        shiny::h4(tags$a("Find out more about this approach for visualising risk", href='https://edjnet.github.io/riskviewer/articles/introduction.html')),
        width = 2),
      mainPanel = mainPanel({
        
        plotOutput(outputId = "airplane_gg", height = 1024)
        
      }))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'riskviewer'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

