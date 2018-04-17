#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# read in data
weather <- read.csv('weather2.csv')

# Content of Page
ui <- fluidPage(
  titlePanel('2018 Data Expo'),
  leafletOutput('distances'),
  plotOutput('hist'),
  HTML('<p>Eric Mckiney and Cameron Zabriskie</p>')
  )

# Server Information
server <- function(input, output) {
  
  # Distance plot 
  output$distances <- renderLeaflet({
    leaflet(weather) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      # Heat map
      # addHeatmap(data = all.orders,
      #            lng = ~longitude, 
      #            lat = ~latitude, 
      #            intensity = ~quantity,
      #            blur = 20, 
      #            max = 500, 
      #            radius = 10,
      #            group = 'heatmap') %>%
      # addLayersControl(
      #   overlayGroups = c('heatmap'),
      #   options = layersControlOptions(collapsed = FALSE)
      # )        %>%
      
      addCircles(lng = ~longitude, 
                 lat = ~latitude, 
                 fillColor = 'red', 
                 radius = 25000, 
                 stroke = FALSE, 
                 fillOpacity = 0.8, 
                 opacity = 0.01
                 )
  })
  
  # output$hist <- renderPlot({
  #   plot(days.orders$orders, days.orders$numDays,
  #        xlab = 'Average Orders',
  #        ylab = 'Delivery Days',
  #        main = 'Delivery Days vs. Average Number of Orders')
  # })
}

shinyApp(ui = ui, server = server)

