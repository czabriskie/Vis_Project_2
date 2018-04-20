#  used https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/

library(shiny)
library(leaflet)

server <- function(input, output) {
  
  weather <- read.csv('weather2.csv')
  
  weather$Date <- as.Date(weather$Date)
  
  cities.states <- weather %>% select(city, state, longitude, latitude) %>% distinct
  cities.states <- droplevels(cities.states)

  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with cities as markers
  output$map <- renderLeaflet({
    leaflet() %>% 
      # addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(data=cities.states, 
                       ~longitude , ~latitude, 
                       layerId=~as.character(paste(city, state, sep = ', ')), 
                       label = ~as.character(paste(city, state, sep = ', ')), 
                       radius=8 , color="black",  
                       fillColor="red", 
                       stroke = TRUE, 
                       fillOpacity = 0.8)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Make a plot based on selected point
  output$plot=renderPlot({
    place <- data_of_click$clickedMarker$id
    if (is.null(place)) {
      place <- 'Salt Lake City, Utah'
    }
    wd <- weather %>% filter(city == gsub(',', '', regmatches(place, regexpr('.+,', place))),
                             state == gsub(', ', '', regmatches(place, regexpr(',.+', place)))) %>% 
      select(Mean_TemperatureF, Date)
    
    plot(wd$Date, wd$Mean_TemperatureF, type = 'l')
  })
}


ui <- fluidPage(
  titlePanel('2018 Data Expo'),

  leafletOutput("map"),
  plotOutput("plot"),

  HTML('<p>Eric Mckiney and Cameron Zabriskie</p>')
)

shinyApp(ui = ui, server = server)