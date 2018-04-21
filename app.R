#  used this link to figure out how buttons work
# https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/

library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(gridExtra)
 
theme_update(plot.title = element_text(hjust = 0.5))

# read in data
weather <- read.csv('weather2.csv')

weather <- na.omit(weather)

weather$Date <- as.character(weather$Date)
weather$Date <- as.Date(weather$Date)

cities.states <- weather %>% select(city, state, longitude, latitude) %>% distinct()

# Content of Page
ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel('2018 Data Expo Shiny App'),
  br(),
  sidebarLayout(
    mainPanel(
      leafletOutput('map', height='400px'),
      absolutePanel(top = -10, left = 70, textInput('target_zone', '' , 'Ex: Salt Lake City'))
      ),
    sidebarPanel(
      radioButtons("feature", h3("Data to Display"),
                         c("Temperature" = "temp",
                           "Humidity" = "humid",
                           "Wind Speed" = "wind.speed",
                           "Precipitation" = "precip"),
                         selected = "temp"),
      br(),
      sliderInput("dateslider",
                  label = h3("Date Range"),
                  min = as.Date("2014-07-01"),
                  max = as.Date("2017-09-01"),
                  value = as.Date(c("2015-01-01", "2015-06-01"))),
      br()
      ), position = "left"),
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
      LAT <- 47
      LONG <- -105
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
    city.data <- weather %>% filter(city == gsub(',', '', regmatches(place, regexpr('.+,', place))),
                                    state == gsub(', ', '', regmatches(place, regexpr(',.+', place)))) 
    state.data <- weather %>% filter( state == gsub(', ', '', regmatches(place, regexpr(',.+', place))))  %>% 
      select(-state) %>% group_by(Date) %>% select(-c(city, Events, AirPtCd)) %>% summarise_all(mean, na.rm = TRUE) 
    
    plots <- list(NULL, NULL, NULL, NULL)
    inputs <- c()
    
    for (i in 1:3){
      if (input$feature != 'temp' && is.null(plots[[i]]) && !('temp' %in% inputs)){
        plots[[i]] <- ggplot(data = city.data, aes(x = Date, y = Mean_TemperatureF)) +
          geom_ribbon(aes(ymin = Min_TemperatureF, ymax = Max_TemperatureF),
                      fill = brewer.pal(5, "Set2")[1], alpha = 0.7) +
          geom_ribbon(aes(ymin = Min_TemperatureF, ymax = Max_TemperatureF), data = state.data, alpha = 0.7,
                      fill = brewer.pal(5, "Set2")[2]) +
          geom_line() +
          geom_line(data = state.data, col = 'red') +
          scale_x_date(limits = as.Date(c(input$dateslider))) +
          labs(title = "Max, Mean, and Min Temperatures",
               x = "",
               y = "Temperature (in Fahrenheit)")
        
          inputs <- c(inputs, 'temp')
      }
      if(input$feature != 'humid'  && is.null(plots[[i]]) && !('humid' %in% inputs)){
        plots[[i]]  <- ggplot(data = weather, aes(x = Date, y = Mean_Humidity)) +
          geom_ribbon(aes(ymin = Min_Humidity, ymax = Max_Humidity),
                      fill = brewer.pal(5, "Set2")[3], alpha = 0.7) +
          geom_line() +
          scale_x_date(limits = as.Date(c(input$dateslider))) +
          labs(title = "Max, Mean, and Min Humidity",
               x = "",
               y = "Humidity (as a Percentage)")
        
        inputs <- c(inputs, 'humid')
      }
      if(input$feature != 'wind.speed' && is.null(plots[[i]]) && !('wind.speed' %in% inputs)){
        plots[[i]]  <- ggplot(data = weather, aes(x = Date, y = Mean_Wind_SpeedMPH)) +
          geom_line() +
          scale_x_date(limits = as.Date(c(input$dateslider))) +
          labs(title = "Max, Mean, and Min Wind Speed",
               x = "",
               y = "Wind Speed (in MPH)")
        
        inputs <- c(inputs, 'wind.speed')
      }
      if(input$feature != 'precip'  && is.null(plots[[i]]) && !('precip' %in% inputs)){
        plots[[i]]  <- ggplot(data = weather, aes(x = Date, y = PrecipitationIn)) +
          geom_line() +
          scale_x_date(limits = as.Date(c(input$dateslider))) +
          labs(title = "Daily Precipitation",
               x = "",
               y = "Precipitation (in Inches)")
        
        inputs <- c(inputs, 'precip')
      }
      
    }
    
    if (input$feature == 'temp'){
      plots[[4]] <- ggplot(data = city.data, aes(x = Date, y = Mean_TemperatureF)) +
                     geom_ribbon(aes(ymin = Min_TemperatureF, ymax = Max_TemperatureF),
                                 fill = brewer.pal(5, "Set2")[1], alpha = 0.7) +
                     geom_ribbon(aes(ymin = Min_TemperatureF, ymax = Max_TemperatureF), data = state.data, alpha = 0.7,
                                 fill = brewer.pal(5, "Set2")[2]) +
                     geom_line() +
                     geom_line(data = state.data, col = 'red') +
                     scale_x_date(limits = as.Date(c(input$dateslider))) +
                     labs(title = "Max, Mean, and Min Temperatures",
                          x = "",
                          y = "Temperature (in Fahrenheit)")
    }
    else if(input$feature == 'humid'){
      plots[[4]] <- ggplot(data = weather, aes(x = Date, y = Mean_Humidity)) +
                     geom_ribbon(aes(ymin = Min_Humidity, ymax = Max_Humidity),
                                 fill = brewer.pal(5, "Set2")[3], alpha = 0.7) +
                     geom_line() +
                     scale_x_date(limits = as.Date(c(input$dateslider))) +
                     labs(title = "Max, Mean, and Min Humidity",
                          x = "",
                          y = "Humidity (as a Percentage)")
    }
    else if(input$feature == 'wind.speed' ){
      plots[[4]] <- ggplot(data = weather, aes(x = Date, y = Mean_Wind_SpeedMPH)) +
                     geom_line() +
                     scale_x_date(limits = as.Date(c(input$dateslider))) +
                     labs(title = "Max, Mean, and Min Wind Speed",
                          x = "",
                          y = "Wind Speed (in MPH)")
    }
    else if(input$feature == 'precip'){
      plots[[4]] <- ggplot(data = weather, aes(x = Date, y = PrecipitationIn)) +
                     geom_line() +
                     scale_x_date(limits = as.Date(c(input$dateslider))) +
                     labs(title = "Daily Precipitation",
                          x = "",
                          y = "Precipitation (in Inches)")
    }
    
    lay <- rbind(c(4, 4, 4, 1, 2),
                 c(4, 4, 4, 3, 3))

    grid.arrange(grobs = plots, layout_matrix = lay)


    })
}

shinyApp(ui = ui, server = server)