ui <- basicPage(
  plotOutput("plot", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  
  # use render image instead https://shiny.rstudio.com/articles/images.html
  
  output$plot <- renderPlot({
    rv_create_arena()
  }, res = 96, width = 1024, height = 1024)
  
  output$info <- renderPrint({
    req(input$plot_click)
    x <- round(input$plot_click$x, 2)
    y <- round(input$plot_click$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
}

shinyApp(ui, server)