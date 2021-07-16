library(shiny)
library(tidyverse)
library(lubridate)
library(Hmisc)

# Only consider files with names that match ddf.YYYY-MM-DD.RData
files <- file.info(
  list.files(pattern = "^ddf[.][0-9]{4}\\-[0-9]{2}\\-[0-9]{2}[.]RData$"))

mostRecent <- files %>% filter(mtime == max(mtime))

ddf <- readRDS(rownames(mostRecent)) #ddf is a Daily Data Frame ("one I prepared earlier" haha) which has clean data
# This app will pick the first .Rdata file in the working directory that it finds


plot_ribbon <- function(df, date, siteLoc, period) {#period :: Int, site :: String, date :: Date, ddf :: data.frame
  filter(df, between(Date, date, date + days(period)), site == siteLoc) %>%
    ggplot() +
    geom_smooth(stat = 'summary', alpha = 0.5, fill = "gray",
                mapping = aes(Date, soil_mint_1),
                fun.data = median_hilow, fun.args = list(conf.int = 0.5)) + #conf.int 0.5 should be the IQR...
    
    # TODO add rainfall on the plot somehow
    
    ylab("minimum soil temperature (°C)") +
    
    theme_minimal()
}


ui <- fluidPage(
  h1("Predicting Soil Temps"),
  
  tabsetPanel(
    tabPanel(
      "Plot",
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
      
      plotOutput(
        "plot",
        hover = hoverOpts(
          id = "plot_hover", delay = 100, delayType = "debounce")
      ),
      textOutput("x.pos"),
      textOutput("y.pos"),
      p("Blue line is the median of predicted soil min temp"),
      p("Grey area is the median 75% of predicted soil min temp"),
      
      
      tags$footer(
        tags$p("Last updated", mostRecent$mtime),
        tags$p("Disclaimer: experimental use only"))
    ),
    tabPanel(
      "About",
      p("For each site and date, BOM provides 33 ensemble members. \
                  BOM updates them daily.
                  An ensemble member is a multi-week weather forecast with \
                  initial conditions that are slightly different to all the other \
                  ensemble members. \
                  This app plots the median minimum soil temp of the emembers \
                  for each site and date (blue line), \
                  and the median 75% of minimum soil temps (shaded area)"),
      
      
      tags$footer(
        tags$a(href = "https://github.com/lindenwells/winter-research",
        "View this project on GitHub")
      )       
    )
  )
)                


server <- function(input, output) {
  
  # values <- reactiveValues(loc = 0)
  # 
  # observeEvent(input$plot_hover$x, {
  #   values$loc <- input$plot_hover$x
  # })
  
  output$plot <- renderPlot({
    plot_ribbon(ddf, input$date, input$site, input$period)
  })
  
  output$x.pos <- renderText({input$plot_hover$x})
  output$y.pos <- renderText({input$plot_hover$y})
  
}

shinyApp(ui = ui, server = server)