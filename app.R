library(shiny)
library(tidyverse)
library(lubridate)
library(Hmisc)
library(leaflet)


sites<-read.csv("EarlySowingSites.csv", stringsAsFactors = F)

# Only consider files with names that match ddf.YYYY-MM-DD.RData
files <- file.info(
  list.files(pattern = "^ddf[.][0-9]{4}\\-[0-9]{2}\\-[0-9]{2}[.]RData$"))

# The most recent file of forecast data
mostRecent <- files %>% filter(mtime == max(mtime))
ddf.forecast <- readRDS(rownames(mostRecent)) # Daily Data Frame which has forecast data

ddf.hindcast <- readRDS("ddf.RData") # hindcast data ('1990:12)

plot_ribbon <- function(date, siteLoc, period) {
  fc <- filter(ddf.forecast, between(Date, date, date + days(period)), site == siteLoc)
  hcDates <- unique(ddf.hindcast$sowdate)
  hcDates <- sort(as.Date(paste( hcDates, format.Date(date, "%Y"), sep = "-"), format="%d-%b-%Y"))
  x <- date - hcDates
  hcDate <- tolower(format.Date(hcDates[ which.min(x[ x > 0]) ], format="%d-%b"))
  print(paste0("hindcast date = ", hcDate))
  
  hc <- filter (ddf.hindcast, sowdate == hcDate & src == "Pred", site == tolower(siteLoc)) %>% 
    mutate(dateNorm = as.Date(paste0(format.Date(Date, "%d/%m"),
                                    "/", format.Date(date, "%Y")), format="%d/%m/%Y")) %>%
    filter(between(dateNorm, date, date + days(period)))
 
  #print(paste0("hindcast date =", hcDate, ", n=",nrow(hc)))
  ggplot() +
    geom_boxplot(data=hc, aes(x=dateNorm, y=soil_mint_1, group=dateNorm), outlier.shape = NA)  +
    geom_smooth(data = fc, stat = 'summary', alpha = 0.65, fill = "gray",
                mapping = aes(Date, soil_mint_1),
                fun.data = median_hilow, fun.args = list(conf.int = 0.5)) + #conf.int 0.5 should be the IQR...
    # TODO add rainfall on the plot somehow
    labs(title=paste("Minimum soil temperature at ", siteLoc),  
         y="Minimum Soil Temperature (°C)",
         x="") +
    theme_minimal()
}


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
        dateInput("date", label="Choose a date to start at", 
                  format = "dd/mm/yy",
                  min= min(files[,"mtime"]), 
                  max= max(files[,"mtime"])),
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
        tags$p("Last updated", mostRecent$mtime),
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
  
  output$plot <- renderPlot({
    plot_ribbon(input$date, input$site, input$period)
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