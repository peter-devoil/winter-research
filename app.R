library(shiny)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(leaflet)

dbg<- function(...) cat(sprintf(...), sep='', file=stderr())
sites<-read.csv("EarlySowingSites.csv", stringsAsFactors = F)

# Only consider files with names that match ddf.YYYY-MM-DD.RData
files <-list.files(pattern = "^ddf[.][0-9]{4}\\-[0-9]{2}\\-[0-9]{2}[.]RData$")

lastUpdateTime <- (file.info(files) %>% filter(mtime == max(mtime)))$mtime
forecastDates <- as.Date(gsub(".RData", "", gsub("ddf.","", files, fixed=T), fixed=T))

minForecastDate <- min(forecastDates)
forecastDate <- maxForecastDate <- max(forecastDates)

hcDir <- "./" # ../../Soil Temperature/
load(paste0(hcDir, "Hindcast.sowdates.RData"))

ui <- fluidPage(
  h3("Soil Temperature Prediction"),
  
  tabsetPanel(
    tabPanel(
      "Minimum soil temperature",
      sidebarPanel(
        tags$head(tags$style(type='text/css', ".well {max-width: 300px;
                                                                margin: auto;}")),
        leafletOutput("map", height=300),
        selectInput("site", label="Site", sites),
#        dateInput("date", label="Choose a date to start at", 
#                  format = "dd/mm/yy",
#                  min= minForecastDate, 
#                  max= maxForecastDate,
#                  value = maxForecastDate),
        radioButtons("period", "Forecast for how long?",
                     choiceNames = c("2 weeks", "4 weeks", "6 weeks"),
                     choiceValues = c(14, 28, 42),
                     selected = 28)
      ),
      mainPanel(      
        plotOutput("plot"),
        p("Blue line is the median of predicted soil min temp"),
        p("Grey area is 75% of predicted soil min temp"),
        p("Boxes are the long term average"),
      ),      
      tags$footer(
        tags$p("Last updated", lastUpdateTime),
        tags$p("Disclaimer: for experimental use only"))
      ),
    tabPanel(
      "About",
      p(HTML("These forecasts present soil temperature modelled by APSIM coupled with ", 
             "the ACCESS-S climate model. Data from ACCESS-S is updated daily from BoM. ",
             "APSIM is run at a set of nominated sites and dates with this forecast data, ", 
             "developing an ensemble of 33 members.")),
      p(HTML("The ensemble can be seen as a multi-week ",
             "weather forecast that can be compared to the long term average. ",
             "While none of the ensemble members are regarded as a &quot;true&quot; ",
             "forecast, thier collective behaviour can be interpreted as a probability ",
             " of deviation from long term meean conditions.")),
      p(HTML("This app plots the median minimum soil temp of the ensemble members ",
             "(blue line), the 75th percentile as a shaded area, ",
             "and the long term average as box plots.")),
      
      tags$footer(
        tags$a(href = "https://github.com/lindenwells/winter-research",
        "View this project on GitHub")
      )       
    )
  )
)                


server <- function(input, output, session) {
  observeEvent(input$map_marker_click, { 
    p <- input$map_marker_click
    if (!is.null(p$id)) {
      updateSelectInput(session, "site", selected = p$id)
    }
  })

  thisYear <- reactive({return(format.Date(forecastDate, "%Y"))})

  forecast.site <- reactive({
    # The most recent file of forecast data
    inputFile<- paste0("ddf.", forecastDate, ".RData")
    ddf <- readRDS(inputFile) # Daily Data Frame which has forecast data
    dbg(paste0("read ", nrow(ddf), " lines from ", inputFile, "\n"))
    return(ddf)
  })
  
  forecast <- reactive({
    ddf <- forecast.site()
    return(ddf[ddf$site ==  input$site & between(ddf$Date, forecastDate, forecastDate + days(input$period)),])
  })
  
  hindcast.site <- reactive({
     if (file.exists(paste0(hcDir, "Hindcast.", tolower(input$site), ".RData"))) {
       load(paste0(hcDir, "Hindcast.", tolower(input$site), ".RData"))
       ddf.hindcast$Date<- as.Date(ddf.hindcast$Date, format="%d/%m/%Y")
       ddf.hindcast$src<- ifelse(ddf.hindcast$emember=="obs" , "Obs", "Pred")
       return(ddf.hindcast)
     } 
     #dbg(paste0("missing file ", hcDir, "Hindcast.", tolower(input$site), ".RData"))
     return(NULL)
  })
  
  hindcast <- reactive({
    theseDates <- paste( sowDates, thisYear(), sep = "-")
    hcDates <- sort(as.Date(theseDates, format="%d-%b-%Y"))
    x <- forecastDate - hcDates
    hcDate <- tolower(format.Date(hcDates[ which.min(x[ x > 0]) ], format="%d-%b"))
    hcDate <- sub("^0+", "", hcDate) # remove leading zero
    #dbg(paste0("hindcast date = ", hcDate,"\n"))
    forecastYear <- format.Date(forecastDate, "%Y")
    if (length(hcDate) == 0) {
      result <- NULL
    } else {
      result <- hindcast.site() %>% 
        filter(src == "Pred" & sowdate == hcDate )  %>% 
        mutate(dateNorm = as.Date(
          paste0(format.Date(Date, "%d/%m"), "/", forecastYear), format="%d/%m/%Y")) %>%
        filter(between(dateNorm, forecastDate, forecastDate + days(input$period)))
      dbg(paste0("hc2 rows  = ", nrow(result),"\n"))
    }
    return(result)
  })
  
  output$plot <- renderPlot({
    fc <- forecast()
    hc <- hindcast()
    g <- ggplot() 
    if (!is.null(hc)) {
      g <- g + geom_boxplot(data = hc, aes(x=dateNorm, y=soil_mint_1, group=dateNorm), outlier.shape = NA)  
    }
    if (!is.null(fc)) {
      g <- g + geom_smooth(data = fc, stat = 'summary', alpha = 0.65, fill = "gray",
                  mapping = aes(Date, soil_mint_1),
                  fun.data = median_hilow, fun.args = list(conf.int = 0.5))  #conf.int 0.5 should be the IQR...
    }
    g <- g + geom_hline(yintercept=12, colour="red3") 
    if (!is.null(hc)) {
      g <- g + geom_text(aes(x=max(hc$dateNorm), y=12, label = "12°C"), colour="red3", hjust="right", nudge_y=0.4) 
    }
    # TODO add rainfall on the plot somehow
    g <- g + labs(title=paste("Minimum soil temperature at ", input$site),  
           y="Minimum Soil Temperature (°C)",
           x="") +
      theme_minimal()
    return(g)
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl=FALSE)) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = sites, ~long, ~lat, label=~Name, layerId=~Name,
                 labelOptions = labelOptions(noHide = T, textOnly = TRUE))
  })
}

shinyApp(ui = ui, server = server)
