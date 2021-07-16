
# open forecastDays.txt or throw an error if not there
# split on commas
# parse YYYYMMDD to dates
# remove any files that don't match ddf.YYYY-MM-DD.RData, printing what you remove.

# On the cron job:
#   copy forecastDays from peter's workstation to nectar
#   run this script
library(lubridate)
fileNamer <- function(date) {
  return(paste0("ddf.", date, ".RData"))
}

fDays <- readLines("forecastDays.tyt") #TODO what if this fails?
stopifnot(length(fDays) == 1)
fDays <- strsplit(fDays, split = ",")
dates <- lapply(fDays, ymd)
keepFiles <- lapply(dates, fileNamer)
files <- list.files(pattern = "^ddf[.][[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}[.]RData$")

for (file in files) {
  if (!file %in% keepFiles) {
    file.remove(file)
    print(paste("Removed", file))
  }
}
