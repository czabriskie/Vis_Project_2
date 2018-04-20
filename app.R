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
library(ggplot2)
library(RColorBrewer)

theme_update(plot.title = element_text(hjust = 0.5))

# read in data
# weather <- read.csv('weather2.csv')
weather <- read.csv('weathertest.csv')

weather$Date <- as.character(weather$Date)
weather$Date <- as.Date(weather$Date)

# Content of Page
ui <- fluidPage(
  titlePanel('2018 Data Expo'),
  leafletOutput('distances'),
  sliderInput("dateslider",
              label = h3("Date Range"),
              min = as.Date("2014-07-01"),
              max = as.Date("2017-09-01"),
              value = as.Date(c("2015-01-01", "2015-06-01"))),
  plotOutput('hist'),
  HTML('<p>Eric McKiney and Cameron Zabriskie</p>')
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
                 # radius = 25000, 
                 stroke = FALSE, 
                 fillOpacity = 0.8, 
                 opacity = 0.01
                 )
  })
  
  output$hist <- renderPlot({
    ggplot(data = weather, aes(x = Date, y = Mean_TemperatureF)) +
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

