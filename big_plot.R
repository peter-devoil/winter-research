setwd("~/Linden/Soil Temperature")

# Daily
ddf<-do.call(rbind, lapply(list.files(path="apsimData", "^Sorghum_[0-9].*Daily.out$"), read.met))
ddf$Date <- as.Date(ddf$Date,"%d/%m/%Y")
ddf$year<-as.numeric(format.Date(ddf$Date, format="%Y"))
ddf$src<- factor(ifelse(ddf$emember=="obs" , "Obs", "Pred"), levels=c("Obs", "Pred"))
ddf$daysFromStart <- as.integer(
  as.Date(paste0("2001/", format.Date(ddf$Date, format="%m/%d")),"%Y/%m/%d")
  - as.Date(paste0("2001-", ddf$sowdate), "%Y-%d-%b"))

thisYear<-1994
thisSite<- "surat"
thisSowdate<-"15-aug"

relevant.ddf <- filter(ddf, year == thisYear & site == thisSite & daysFromStart <= 22 & sowdate == thisSowdate)

ggplot(data = relevant.ddf) +
  geom_boxplot(mapping = aes(daysFromStart, soil_temp_1, group = daysFromStart), color = "green") +
  geom_boxplot(mapping = aes(daysFromStart, soil_mint_1, group = daysFromStart), color = "blue") +
  facet_grid(.~sowdate)

