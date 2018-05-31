library(shiny)
library(shinythemes)

## -------------------------------------- UI ------------------------------------------

ui <- shinyUI(
  fluidPage(
    theme = shinytheme("slate"),
    navbarPage(title = "Pancreas Disease in Norwegian Fish"),
    sidebarLayout(
      position = "left",
      sidebarPanel(
        fluidRow(
          column(4,checkboxGroupInput("artnavn", "Species:",
                                      choices = unique(stats_df$artnavn))),
          column(3,checkboxGroupInput("aar", "Year:",
                                      choices = unique(stats_df$aar)))
      )),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot"),
                    tabPanel("Data"),
                    tabPanel("Code"))
      )
    )
  )
  
)



## ----------------------------------- Server -------------------------------------------
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run application
shinyApp(ui = ui, server = server)

