#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
rv_explorer_app_ui <- function(request) {
  tagList(
    
    golem_add_external_resources(),
    HTML("<!-- Global site tag (gtag.js) - Google Analytics -->
  <script async src='https://www.googletagmanager.com/gtag/js?id=UA-109815904-1'></script>
    <script>
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());
    
    gtag('config', 'UA-109815904-1');
    </script>
      "),
    tags$style("h3 a {color: #6f2c91;}"),
    tags$style("p a {color: #6f2c91;}"),
    tags$style("h3 a:hover {color: #a6ce39;}"),
    tags$style("p a:hover {color: #a6ce39;}"),
    fluidPage(
      theme = bslib::bs_theme(
        version = 4,
        bootswatch = "journal",
        bg = "white",
        fg = "black",
        primary = "#2780E3"
      ),
      shiny::sidebarLayout(sidebarPanel = sidebarPanel(
        mod_rv_create_airplane_compact_ui("rv_create_compact_airplane_1"),
        shiny::hr(),
        shiny::h3(tags$a("A tool by EDJNet", href='https://github.com/edjnet/riskviewer/')),
        shiny::h3(tags$a(tags$img(src = "www/img/edjnet_logo_full.svg"),
                         href='https://www.europeandatajournalism.eu/')),
        shiny::p(tags$a("Find out more about this approach for visualising risk",
                        href='https://edjnet.github.io/riskviewer/articles/introduction.html')),
        width = 2),
        mainPanel = mainPanel(
          plotOutput(outputId = "airplane_gg", height = 1024)
          
        ))
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
      app_title = 'Riskviewer explorer - A tool by EDJNet'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

