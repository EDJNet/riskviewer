#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
rv_explorer <- function(
    ...) {
  with_golem_options(
    app = shinyApp(
      ui = rv_explorer_app_ui,
      server = rv_explorer_app_server
    ),
    golem_opts = list(...)
  )
}
