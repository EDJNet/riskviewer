#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
rv_explorer_app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  output$airplane_gg <- mod_rv_create_airplane_compact_server("rv_create_compact_airplane_1")

}
