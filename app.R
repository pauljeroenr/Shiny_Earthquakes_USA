library("shinydashboard")
library("shiny")
library("leaflet")
library("tidyverse")
#comment out when publishing
#setwd("C:/Users/paul_/OneDrive/Desktop/master/r_scripts")
ui <- dashboardPage(
  dashboardHeader(title = "Earthquakes for USA"),
  dashboardSidebar(),
  dashboardBody(
    #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      box(leafletOutput("map"), width = "100%", height = "100%")
    )
  )
)

server <- function(input, output, session) {
  day_long <- reactive({
    invalidateLater(1000*60*10, session)
    source("earthquake_dashboard.R")
  })
  
  
  output$map <- renderLeaflet({
    plt <- 
    leaflet() %>% 
    addTiles() %>% 
    addCircles(lng = unlist(day_long()$value$lng), lat = unlist(day_long()$value$lat))
  })
}

shinyApp(ui, server)
