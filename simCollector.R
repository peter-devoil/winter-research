library(tibble)
library(lubridate)
read.out <- function(out.name) {
  header<-readLines(out.name, n=5)
  i <- grep("^Title = *", trimws(header), ignore.case=T)
  if (i <= 0) {return(NULL)}
  sim <- read.table(out.name, skip = i + 2, na.strings=c("NA","?"))
  names(sim) <- unlist(strsplit(trimws(header[i + 1]), " +"))
  sim$Date <- as.Date(sim$Date,"%d/%m/%Y")
  
  dummy <- unlist(strsplit(out.name, "\\."))
  if (length(dummy) != 4) {stop(paste0("funny filename: ", out.name, " Pls make sure it's in the format YYYYMMDD.sitename.e%d.out"))}
  forecastDate <- dummy[[1]]
  site <- dummy[[2]]
  emember <- dummy[[3]]
  
  sim <- sim %>% add_column(forecastDate = replicate(nrow(sim), forecastDate))
  sim <- sim %>% add_column(site = replicate(nrow(sim), site))
  sim <- sim %>% add_column(emember = replicate(nrow(sim), emember))
  return(sim)
}

# Should match filenames of the format "YYYYMMDD.sitename.eXX.out".
# FIXME why can't I prepend the below pattern with '^'? Confused.
ddf <- do.call(rbind, lapply(list.files(path=".", "[[:digit:]]{6}[.]\\w+[.]e[[:digit:]]{2}[.]out$"), read.out))

saveRDS(ddf, paste0("ddf.", today(), ".RData")) #TODO beware this gets today's date, NOT necessarily the date of the forecasts.

