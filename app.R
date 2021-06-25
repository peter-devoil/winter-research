library(shiny)
library(tidyverse)

plot_ribbon <- function(ddf, date, site, period) {#period :: Int, site :: String, date :: Date, ddf :: data.frame
  filter(ddf, between(Date, date, date + days(period)), site == thisSite) %>%
    ggplot() +
    geom_smooth(stat = 'summary', alpha = 0.5, fill = "gray", mapping = aes(daysFromStart, soil_mint_1),
                fun.data = median_hilow, fun.args = list(conf.int = 0.5))
}


ui <- fluidPage("Predicting Soil Temps",
                selectInput("site", label="Choose a site",
                            c("breeza", "dalby", "dubbo","emerald", "moree", "surat", "warra")),
                textOutput("result"))

server <- function(input, output) {
  
  output$result <- renderText({
    paste("You chose", input$site)
  })
  
}

shinyApp(ui = ui, server = server)