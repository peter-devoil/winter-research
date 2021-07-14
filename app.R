library(shiny)
library(tidyverse)
library(lubridate)
library(Hmisc)
files <- list.files(pattern = "[.]RData")
ddf <- readRDS(files[1]) #ddf is a Daily Data Frame ("one I prepared earlier" haha) which has clean data
# This app will pick the first .Rdata file in the working directory that it finds

plot_ribbon <- function(df, date, siteLoc, period) {#period :: Int, site :: String, date :: Date, ddf :: data.frame
  filter(df, between(Date, date, date + days(period)), site == siteLoc) %>%
    ggplot() +
    geom_smooth(stat = 'summary', alpha = 0.5, fill = "gray", mapping = aes(Date, soil_mint_1),
                fun.data = median_hilow, fun.args = list(conf.int = 0.5)) + #conf.int 0.5 should be the IQR...
    
    ylab("minimum soil temperature (°C)") +
    
    theme_minimal()
}


ui <- fluidPage(h1("Predicting Soil Temps"),
                
                wellPanel(
                  tags$head(tags$style(type='text/css', ".well {max-width: 300px;
                                                                margin: auto;}")),
                  selectInput("site", label="Choose a site",
                              unique(ddf$site),
                  ),
                  dateInput("date", label="Choose a date to start at"),
                  radioButtons("period", "Forecast for how long?",
                               choiceNames = c("1 week", "2 weeks", "3 weeks", "4 weeks"),
                               choiceValues = c(7, 14, 21, 30))
                ),
                
                plotOutput("plot"),
                p("Blue line is the median of predicted soil min temp"),
                p("Grey area is the median 75% of predicted soil min temp"),
                
                tags$footer(
                  tags$a(href = "https://github.com/lindenwells/winter-research", "View this project on GitHub"))
                )

server <- function(input, output) {
  
  output$plot <- renderPlot({
    plot_ribbon(ddf, input$date, input$site, input$period)
  })
  
}

shinyApp(ui = ui, server = server)