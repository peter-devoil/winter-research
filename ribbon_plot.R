setwd("~/Linden/Soil Temperature/apsimData")
library(tidyverse)
library(lubridate)

read.met <- function(met.name) {
  header<-readLines(met.name, n=25)
  i<-grep("^year*|^Date*", trimws(header), ignore.case=T)
  if (i <= 0) {return(NULL)}
  met <- read.table(met.name,skip=i+2,na.strings=c("NA","?"))
  names(met) <- unlist(strsplit(trimws(header[i]), " +"))
  return(met)
}

# Daily
# ddf<-do.call(rbind, lapply(list.files(path=".", "^Sorghum_[0-9].*Daily.out$"), read.met))
# ddf$Date <- as.Date(ddf$Date,"%d/%m/%Y")
# ddf$year<-as.numeric(format.Date(ddf$Date, format="%Y"))
# ddf$src<- factor(ifelse(ddf$emember=="obs" , "Obs", "Pred"), levels=c("Obs", "Pred"))
# ddf$daysFromStart <- as.integer(
#   as.Date(paste0("2001/", format.Date(ddf$Date, format="%m/%d")),"%Y/%m/%d")
#   - as.Date(paste0("2001-", ddf$sowdate), "%Y-%d-%b"))
# ddf$sowdate <- factor(ddf$sowdate, levels = c("15-jun", "1-jul", "15-jul", "1-aug","15-aug"))

thisYear<-1994
thisSite<- "dubbo"
thisSowdate<-"15-aug"

plot_ribbon <- function(ddf, date, siteLoc, period) {#period :: Int, site :: String, date :: Date, ddf :: data.frame
  filter(ddf, between(Date, date, date + days(period)), site == siteLoc) %>%
  ggplot() +
    geom_smooth(stat = 'summary', alpha = 0.5, fill = "gray", mapping = aes(daysFromStart, soil_mint_1),
                fun.data = median_hilow, fun.args = list(conf.int = 0.5))
}
#plot the median min temp as a line, and the IQR as the grey shaded area.
  

