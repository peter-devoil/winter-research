library(shiny)
library(tidyverse)
library(lubridate)
library(Hmisc)
ddf <- readRDS("ddf.RData") #ddf is a Daily Data Frame ("one I prepared earlier" haha) which has clean data

plot_ribbon <- function(df, date, siteLoc, period) {#period :: Int, site :: String, date :: Date, ddf :: data.frame
  filter(df, between(Date, date, date + days(period)), site == siteLoc) %>%
    ggplot() +
    geom_smooth(stat = 'summary', alpha = 0.5, fill = "gray", mapping = aes(daysFromStart, soil_mint_1),
                fun.data = median_hilow, fun.args = list(conf.int = 0.5)) +
    xlab("days from start") +
    
    ylab("minimum soil temperature (°C)") +
    
    scale_x_continuous(n.breaks = period)
}


ui <- fluidPage(h1("Predicting Soil Temps"),
                
                wellPanel(
                  selectInput("site", label="Choose a site",
                              c("breeza", "dalby", "dubbo","emerald", "moree", "surat", "warra")),
                  dateInput("date", label="Choose a date to start at", value = "1990-06-16"),
                  sliderInput("period", "Forecast for how many days?", 7, 21, 14, round = TRUE)
                ),
                
                plotOutput("plot"),
                p("Blue line is the median of predicted soil min temp"),
                p("Grey area is the interquartile range of predicted soil min temp"),
                
                tags$footer(
                  tags$a(href = "https://github.com/lindenwells/winter-research", "View this project on GitHub"))
                )

server <- function(input, output) {
  
  output$plot <- renderPlot({
    plot_ribbon(ddf, input$date, input$site, input$period)
  })
  
}

shinyApp(ui = ui, server = server)