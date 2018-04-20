# Example Plots

library(ggplot2)
library(RColorBrewer)

weather$Date <- as.character(weather$Date)
weather$Date <- as.Date(weather$Date)

theme_update(plot.title = element_text(hjust = 0.5))

ggplot(data = weather, aes(x = Date, y = Mean_TemperatureF)) +
  geom_ribbon(aes(ymin = Min_TemperatureF, ymax = Max_TemperatureF),
              fill = brewer.pal(5, "Set2")[1]) +
  geom_line() +
  labs(title = "Max, Mean, and Min Temperatures",
       x = "",
       y = "Temperature (in Fahrenheit)")
