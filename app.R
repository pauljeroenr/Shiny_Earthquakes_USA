library("shinydashboard")
library("shiny")
library("leaflet")
library("tidyverse")
library("shinycssloaders")
#comment out when publishing
#setwd("C:/Users/paul_/OneDrive/Desktop/master/r_scripts")
ui <- dashboardPage(
  dashboardHeader(title = "Earthquakes USA"),
  # Two Sidebar Panels for Earthquakes in the last 1 and 24 hour
  dashboardSidebar(
    sidebarMenu(
      menuItem("Past  1 hour", tabName = "1h", icon = icon("clock")),
      menuItem("Past 24 hour", tabName = "24h", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Plot of the different Grahpics
      tabItem(tabName = "1h",
              fluidRow(
                box(withSpinner(leafletOutput("map1", height = "65vh")), height = "70vh", width = "100%"),
                absolutePanel(bottom = 0, left = 250, width = 500, height = 500, draggable = TRUE,
                              valueBoxOutput("num1_earthquake"),
                              valueBoxOutput("strongest1_earthquake")),
                #valueBoxOutput("num1_earthquake", width = 4),
                #valueBoxOutput("strongest1_earthquake", width = 4),
                valueBoxOutput("last1_update", width = 4),
                infoBox(title = "Information", value = "There were no Earthquakes when no map renders"),
                valueBoxOutput("loc1", width = 12))
              ),
      tabItem(tabName = "24h",
              fluidRow(
                box(withSpinner(leafletOutput("map24", height = "65vh")), height = "70vh", width = "100%"),
                absolutePanel(bottom = 0, left = 250, width = 500, height = 500, draggable = TRUE,
                              valueBoxOutput("num24_earthquake"),
                              valueBoxOutput("strongest24_earthquake")),
                valueBoxOutput("last24_update", width = 4),
                infoBox(title = "Information", value = "There were no Earthquakes when no map renders"),
                valueBoxOutput("loc24", width = 12))
              )
    )
  )
)
server <- function(input, output, session) {
  # Get the Date for 24 hour refresh every 10 min
  # And for 1 hour refresh every 5 min
  day_earthquake <- reactive({
    invalidateLater(1000*60*10, session)
    source("earthquake24_dashboard.R")
  })
  hour_earthquake <- reactive({
    invalidateLater(1000*60*5, session)
    source("earthquake1_dashboard.R")
  })
  # Get the Time of the last Refresh
  last24_download <- reactive({
    invalidateLater(1000*60*10, session)
    Sys.time()
  })
  last1_download <- reactive({
    invalidateLater(1000*60*5, session)
    Sys.time()
  })
  ###### Output for the 24 hour Tab ######
  
  # Map with all Earthquakes
  output$map24 <- renderLeaflet({
    leaflet(data = day_earthquake()$value) %>% 
    addProviderTiles(providers$Stamen.TonerLite) %>% 
    addCircles(lng = ~lng, 
               lat = ~lat,
               radius = ~mag * 30000, #radius in meter magnitude between 0-9 
               stroke = FALSE,
               color = "darkred",
               fillOpacity = 0.5)
  })
  # Valuebox with the number of Earthquakes
  output$num24_earthquake <- renderValueBox({
    num_earthquake_df <- 
      day_earthquake()$value %>%
      nrow()
    valueBox(num_earthquake_df,
             "Number of Earthquake in the last 24h",
             color = "purple")
  })
  # Valuebox with the strongest Magnitude
  output$strongest24_earthquake <- renderValueBox({
    max_earthquake <- 
      day_earthquake()$value %>% 
      select(mag) %>% 
      max()
    # Color of box depending on the magnitude 
    var_color <- 
      if (max_earthquake >= 7.0) {color = "red"
      }else if (max_earthquake >= 5.0) {color = "yellow"
      }else {color = "green"}
    valueBox(max_earthquake,
             "Strongest Magnitude in the last 24h",
             color = var_color)
  })
  # Valuebox with the Time of the last Update
  output$last24_update <- renderValueBox({
    valueBox(last24_download(),
             "Last Data Update",
             icon = icon("clock"),
             color = "purple")
  })
  # Location of strongest Earthquake
  output$loc24 <- renderValueBox({
    location <- 
      day_earthquake()$value %>% 
      filter(mag == max(mag)) %>% 
      select(place) %>% 
      unlist()
    valueBox(location[1],
            "Location of strongest Earthquake")
  })
  ##### Output for the 1 hour Tab #####
  
  # Map with all Earthquakes
  output$map1 <- renderLeaflet({
    leaflet(data = hour_earthquake()$value) %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      addCircles(lng = ~lng, 
                 lat = ~lat,
                 radius = ~mag * 30000, #radius in meter magnitude between 0-9 
                 stroke = FALSE,
                 color = "darkred",
                 fillOpacity = 0.5)
  })
  # Valuebox with the number of Earthquakes
  output$num1_earthquake <- renderInfoBox({
    num_earthquake_df <- 
      hour_earthquake()$value %>%
      nrow()
    valueBox(num_earthquake_df,
             "Number of Earthquake in the last hour",
             color = "purple")
  })
  # Valuebox with the strongest Magnitude
  output$strongest1_earthquake <- renderValueBox({
    max_earthquake <- 
      hour_earthquake()$value %>% 
      select(mag) %>% 
      max()
    #Color of Box depending on Magnitude
    var_color <- 
      if (max_earthquake >= 7.0) {color = "red"
      }else if (max_earthquake >= 5.0) {color = "yellow"
      }else {color = "green"}
    valueBox(max_earthquake,
             "Strongest Magnitude in the last hour",
             color = var_color)
  })
  # Valuebox with the time of the last Update
  output$last1_update <- renderValueBox({
    valueBox(last1_download(),
             "Last Data Update",
             icon = icon("clock"),
             color = "purple")
  })
  ## Valuebox with the Location of the strongest Earthquake
  output$loc1 <- renderValueBox({
    location <- 
      hour_earthquake()$value %>% 
      filter(mag == max(mag)) %>% 
      select(place) %>% 
      unlist()
    valueBox(location[1],
             "Location of strongest Earthquake")
  })
}

shinyApp(ui, server)

