library(shiny)
library(tidyverse)
source("ribbon_plot.R")
#This code assumes a data frame called ddf is in the environment already... FIXME

plot_ribbon <- function(ddf, date, siteLoc, period) {#period :: Int, site :: String, date :: Date, ddf :: data.frame
  filter(ddf, between(Date, date, date + days(period)), site == siteLoc) %>%
    ggplot() +
    geom_smooth(stat = 'summary', alpha = 0.5, fill = "gray", mapping = aes(daysFromStart, soil_mint_1),
                fun.data = median_hilow, fun.args = list(conf.int = 0.5))
}


ui <- fluidPage("Predicting Soil Temps",
                selectInput("site", label="Choose a site",
                            c("breeza", "dalby", "dubbo","emerald", "moree", "surat", "warra")),
                dateInput("date", label="Choose a date to start at", value = "1990-06-16"),
                sliderInput("period", "Forecast for how many days?", 7, 21, 14, round = TRUE),
                textOutput("result"),
                plotOutput("plot"))

server <- function(input, output) {
  
  output$plot <- renderPlot({
    plot_ribbon(ddf, input$date, input$site, input$period)
  })
  
}

shinyApp(ui = ui, server = server)