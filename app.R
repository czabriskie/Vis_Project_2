#  used this link to figure out how buttons work
# https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

theme_update(plot.title = element_text(hjust = 0.5))


# weather <- read.csv('weather2.csv')
weather <- read.csv('weather2.csv')


weather$Date <- as.character(weather$Date)
weather$Date <- as.Date(weather$Date)

cities.states <- weather %>% select(city, state, longitude, latitude) %>% distinct

# Content of Page
ui <- fluidPage(
  titlePanel('2018 Data Expo'),
  br(),
  leafletOutput('map', height='600px'),
  absolutePanel(top = 75, left = 70, textInput('target_zone', '' , 'Ex: Salt Lake City')),
  sliderInput("dateslider",
              label = h3("Date Range"),
              min = as.Date("2014-07-01"),
              max = as.Date("2017-09-01"),
              value = as.Date(c("2015-01-01", "2015-06-01"))),
  plotOutput('plot'),
  HTML('<p>Eric McKiney and Cameron Zabriskie</p>')
  )

# Server Information
server <- function(input, output) {
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker = NULL)
  
  # Leaflet map 
  output$map <- renderLeaflet({
    
    # Get latitude and longitude
    if(input$target_zone == 'Ex: Salt Lake City'){
      ZOOM <- 3
      LAT <- 38.075171
      LONG <- -110.560604
    }else{
      target_pos <- geocode(input$target_zone)
      LAT <- target_pos$lat
      LONG <- target_pos$lon
      ZOOM <- 6
    }
    
    leaflet() %>% 
      setView(lng = LONG, lat = LAT, zoom = ZOOM ) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data = cities.states, 
                       ~longitude , ~latitude, 
                       layerId = ~as.character(paste(city, state, sep = ', ')), 
                       label = ~as.character(paste(city, state, sep = ', ')), 
                       radius = 8 , color="black",  
                       fillColor = "red", 
                       stroke = TRUE, 
                       fillOpacity = 0.8)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Make a plot based on selected point
  output$plot <- renderPlot({
    place <- data_of_click$clickedMarker$id
    if (is.null(place)) {
      place <- 'Salt Lake City, Utah'
    }
    wd <- weather %>% filter(city == gsub(',', '', regmatches(place, regexpr('.+,', place))),
                             state == gsub(', ', '', regmatches(place, regexpr(',.+', place)))) %>% 
      select(Mean_TemperatureF, Date, Min_TemperatureF, Max_TemperatureF)
    
    
    ggplot(data = wd, aes(x = Date, y = Mean_TemperatureF)) +
      geom_ribbon(aes(ymin = Min_TemperatureF, ymax = Max_TemperatureF),
                  fill = brewer.pal(5, "Set2")[1]) +
      geom_line() +
      scale_x_date(limits = as.Date(c(input$dateslider))) +
      labs(title = "Max, Mean, and Min Temperatures",
           x = "",
           y = "Temperature (in Fahrenheit)")

    })
}

shinyApp(ui = ui, server = server)