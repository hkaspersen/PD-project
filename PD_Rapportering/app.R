library(shiny)
library(shinythemes)
library(tidyverse)
library(maps)

## -------------------------------------- UI ------------------------------------------

ui <- shinyUI(
  fluidPage(
    theme = shinytheme("slate"),
    navbarPage(title = "Pancreas Disease in Norwegian Fish"),
    sidebarLayout(
      position = "left",
      sidebarPanel(
        fluidRow(
          column(4,checkboxGroupInput("month_name", "Month:",
                                      choices = unique(stats_df$month_name),
                                      selected = "January")),
          column(3,checkboxGroupInput("aar", "Year:",
                                      choices = unique(stats_df$aar),
                                      selected = "2018"))
      )),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot",
                             plotOutput("mapPlot")),
                    tabPanel("Data"),
                    tabPanel("Code"))
      )
    )
  )
  
)



## ----------------------------------- Server -------------------------------------------
server <- function(input, output) {
   
  data <- reactive({
    req(input$month_name)
    req(input$aar)
    
    world <- map_data("world")%>%
      rename(country=region)%>%
      filter(country=="Norway") %>%
      mutate(test = case_when(subregion == "Svalbard" ~ 0,
                              subregion == "Jan Mayen" ~ 0,
                              is.na(subregion) == TRUE ~ 1,
                              TRUE ~ 1)) %>%
      filter(test == 1)
    
    locreg <- read.table("lokreg_oppdatert.txt", sep = "\t", stringsAsFactors = F, header = TRUE) %>%
      mutate(LokNr = as.character(LokNr),
             EastGeo = gsub(",", ".", EastGeo),
             NorthGeo = gsub(",", ".", NorthGeo),
             EastGeo = as.numeric(EastGeo),
             NorthGeo = as.numeric(NorthGeo)) %>%
      rename(long = EastGeo,
             lat = NorthGeo)
    
    mapdata <- stats_df
    
    mapdata_PD <- calc_stats(mapdata, "eier_lokalitetnr", "month_name") %>%
      left_join(., locreg, by = c("eier_lokalitetnr" = "LokNr")) %>%
      mutate(PD = ifelse(Påvist != 0, "Negative", "Positive")) %>%
      filter(aar %in% input$aar,
             month_name %in% input$month_name)
  })
  output$mapPlot <- renderPlot({
     p1 <- ggplot() +
       geom_polygon(data = world, aes(x = long, y = lat, group = group), alpha = 0.6)+
       geom_point(
         data = mapdata_PD,
         aes(
           x = long,
           y = lat,
           fill = PD,
           size = total
         ),
         pch = 21
       )+
       theme_classic()+
       theme(axis.text = element_blank(),
             axis.ticks = element_blank(),
             axis.title = element_blank(),
             axis.line = element_blank())+
       coord_fixed(2)
     print(p1)
   })
}

# Run application
shinyApp(ui = ui, server = server)

