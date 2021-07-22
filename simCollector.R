library(tibble)
library(lubridate)

print(paste("simCollector.R started at", now()))

outFilesDir <- "cache/bom/"

read.out <- function(out.name) {
  header<-readLines(paste0(outFilesDir, out.name), n=5)
  i <- grep("^Title = *", trimws(header), ignore.case=T)
  if (i <= 0) {return(NULL)}
  sim <- read.table(paste0(outFilesDir, out.name), skip = i + 2, na.strings=c("NA","?"))
  names(sim) <- unlist(strsplit(trimws(header[i + 1]), " +"))
  sim$Date <- as.Date(sim$Date,"%d/%m/%Y")
  
  dummy <- unlist(strsplit(out.name, "\\.|_")) # split on . and _
  # SEE BELOW FOR DELICIOUS SPAGHETTI!!
  if (length(dummy) == 4) {
    forecastDate <- dummy[[1]]
    site <- dummy[[2]]
    emember <- dummy[[3]]
  } else if (length(dummy) == 5) {
    forecastDate <- dummy[[1]]
    site <- paste0(dummy[[2]], "_", dummy[[3]])
    emember <- dummy[[4]]
  } else {
    stop(paste0("funny filename: ", out.name, " Pls make sure it's in the format YYYYMMDD_sitename_eXX.out"))
  }
  
  sim <- sim %>% add_column(forecastDate = replicate(nrow(sim), forecastDate))
  sim <- sim %>% add_column(site = replicate(nrow(sim), site))
  sim <- sim %>% add_column(emember = replicate(nrow(sim), emember))
  return(sim)
}

# Should match filenames of the format "YYYYMMDD_sitename_eXX.out".
ddf <- do.call(rbind, lapply(list.files(path = outFilesDir, "[[:digit:]]{8}_\\w+_e[[:digit:]]{2}[.]out$"), read.out))

saveRDS(ddf, paste0(outFilesDir, "ddf.", today(), ".RData")) #TODO beware this gets today's date, NOT necessarily the date of the forecasts.

print(paste("simCollector.R finished at", now()))
