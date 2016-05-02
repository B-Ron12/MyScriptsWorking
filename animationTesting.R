if (interactive()) {
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          p("The first slider controls the second"),
          sliderInput("control", "Controller:", min=1, max=40, value=1,
                      step=1, animate=TRUE)
        ),
        mainPanel(
          leafletOutput("rasterToPlot"
          ))
      )
    ),
    server = function(input, output, session) {
      
      
      observe({
        val <- input$control
        output$rasterToPlot <- renderLeaflet({
          leaflet(BC) %>%
          addRasterImage(agbio_stack[[val]])})
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
      })
    }
  )
}
