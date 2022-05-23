# 1. Get the latest forecast data from bom opendap / file server. 
# 2. Create apsim met files

cat("Starting at ", format.POSIXct(Sys.time()), "\n")

rm(list=ls())
setwd("/home/data/access-s/met/")

# set the longitude and latitude of your station
sites<-read.csv("Allsites.csv", stringsAsFactors = F)

library("ncdf4")
#library(devtools)
#install_github("BigelowLab/thredds")
library(thredds)

# convert w/m2 -> mj/m2
w2m<-function(x) {x * (24 * 60 * 60)/1000000}

# There are 99 members there, we use 33
emembers<-paste0("e",sprintf("%02d", 1:33))
base_uri <-"http://opendap.bom.gov.au:8080/thredds"
top <- thredds::CatalogNode$new(paste0(base_uri, "/catalog/seasonal_prediction/access-s/atmos/catalog.xml"))

cache <- "./cache/bom/"

forecastDays <- vector()
# download any new netcdf files
for (variable in c("pr", "rsds", "tasmax", "tasmin")) {
   cats <- top$get_catalogs(variable)[[variable]]$get_catalogs()
   for (emember in emembers) {
      datas <- cats[[emember]]$get_datasets()
      for (fc in datas) {
         if (grepl("^s_", fc$name)) {
            # want something like:
            # http://opendap.bom.gov.au:8080/thredds/dodsC/seasonal_prediction/access-s/atmos/pr/e03/s_daq5_pr_20210701_e03.nc.html
            # http://opendap.bom.gov.au:8080/thredds/fileServer/seasonal_prediction/access-s/atmos/pr/e03/s_daq5_pr_20210703_e03.nc
            fcastday <- strsplit(fc$name, "_")[[1]][4]

            forecastDays <- unique(c(forecastDays,fcastday))
   
            localFile <- paste0(cache, fc$name)
            if (file.exists(localFile)) { next }
            
            # This fails quite a lot with network errors (each file is ~100Mb), retry 
            # before giving up.
            cnt <- 0; res = 1
            while(res != 0 && cnt < 5) {
               tryCatch(
                 {
                    cat("Downloading ", localFile, "\n")
                    res <- download.file(paste0(base_uri, "/fileServer/", fc$url), localFile, quiet=T)
                 },
                 error=function(x) {cat("Error:\n"); print(x)},
                 warning=function(x) {cat("Warning:\n"); print(x)},
                 finally={cnt <- cnt + 1})
               if (res != 0) { Sys.sleep(sample(10:20,1)) }
            }
         }
      }
   }
}

# Extract our sites of interest from each netcdf file
last<- function(x){return(x[length(x)])}
ncFiles <- list.files(path=cache, glob2rx("*.nc"), full.names = T)
#ncFiles <- list.files(path=cache, glob2rx("s_daq5_*_20220516*.nc"), full.names = T)

for (ncFile in ncFiles) {
   z<- last(strsplit(ncFile, "/")[[1]])
   z<- strsplit(z, "_")[[1]]
   variable <- z[3]
   fcastday <- z[4]
   emember <- gsub(".nc", "", z[5], fixed = T)
   localFile <- paste0(cache, fcastday, ".", variable, ".", emember, ".RData")
   if (file.exists(localFile)) { next }
   
   cat("Extracting from ", ncFile, "\n")
   nc <- ncdf4::nc_open(ncFile)
   
   lon <- ncvar_get(nc,"lon")
   lat <- ncvar_get(nc,"lat")
   daysSince <- ncvar_get(nc,"time")
   if (any(daysSince < 0)) {
      daysSince <- 0:(length(daysSince)-1)
      cat("Holey time axis in ", url, "\n")
   }
   myTime<-as.Date(substr(ncatt_get(nc, "time")$units, 12, 21)) + daysSince
   data<-data.frame(date=myTime)
   
   for (i in 1:nrow(sites)) {
      # find the latitude index of the array corresponding to nearest model value
      dlat <- abs(lat-sites$lat[i])
      ilat <- which(dlat==min(dlat))
      
      # find the latitude index of the array corresponding to nearest model value
      dlon <- abs(lon-sites$long[i])
      ilon <- which(dlon==min(dlon))
      
      # extract quantity from the opendap file
      #cat(localFile, " - ", sites$Name[i], "=", ilon, ",", ilat, "\n")
      data[,i+1] <- as.vector(ncvar_get(nc, variable, c(ilon,ilat,1),c(1,1,length(myTime))))
   }
   names(data)<-c("date",as.character(sites$Name))
   nc_close(nc)
   
   save(data, file = localFile)
}

# get silo data up until yesterday
read.tavamp <- function(met.name) {
   header<-readLines(met.name, n=25)
   tav<- header[grep("^tav", header, ignore.case=T)]
   tav<- as.numeric(unlist(strsplit(tav,"=|\\("))[2])
   amp<- header[grep("^amp", header, ignore.case=T)]
   amp<- as.numeric(unlist(strsplit(amp,"=|\\("))[2])
   return(list(tav=tav, amp=amp))
}

sites$tav<- NA
sites$amp<- NA

for (i in 1:nrow(sites)) {
   metfile <- paste0(cache,sites$Name[i], ".met")
   url<-paste0(
      "https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?",
      "format=apsim&start=19800101&finish=", format.Date(Sys.Date(), format="%Y%m%d"),
      "&station=", sites$Met.Station.ID[i], "&username=peter.devoil@uq.edu.au")
   cat("Downloading ", metfile, "\n")
   download.file(url, metfile,quiet=T)
   tavamp<- read.tavamp(metfile)
   sites$tav[i] <- tavamp$tav
   sites$amp[i] <- tavamp$amp
}

# create metfiles for each site.
localFiles <- list.files(path=cache,glob2rx("*.RData"))
#localFiles <- "cache/bom/ddf.2022-05-18.RData"
fcastDates <- unique(unlist(lapply(strsplit(localFiles, ".", fixed = T), function(x) {x[1]})))
for (fcastDate in fcastDates) {
   for(iSite in 1:nrow(sites)) {
      for (emember in emembers) {
         fileName <- paste0(cache, fcastDate, ".", sites$Name[iSite], ".", emember, ".met")
         if (file.exists(fileName)) { next }
         
         df<- NULL
         for (v in c("pr", "rsds", "tasmax", "tasmin")) {
            x<- environment()
            rDataFile <- paste0(cache, fcastDate, ".", v, ".", emember, ".RData")
            if (file.exists(rDataFile)) {
               cat(paste("Loading", rDataFile, "\n"))
               load(rDataFile, envir = x)
               if (is.null(df)) {
                  # Add date on first time
                  df <- data.frame(date = get("data", envir = x)[, "date"])
               }
               df[[v]] <- get("data", envir = x)[, sites$Name[iSite]]
            }
         }
         if (!is.null(df) && ncol(df) == 5) {
            cat("Writing ", fileName, "\n")
            odf<-data.frame(
               year=format.Date(df$date, format="%Y"),
               day=as.numeric(format.Date(df$date, format="%j")),
               rain=round(df$pr,1),
               maxt=round(df$tasmax, 1),
               mint=round(df$tasmin, 1),
               radn=round(w2m(df$rsds), 1))
         
            cat("# ACCESS-S Forecast met data\n", file = fileName)
            cat("# Experimental use only\n", file = fileName, append = T)
            cat("latitude = ", sites$lat[iSite], "\n", file = fileName, append = T)
            cat("tav = ", sites$tav[iSite], "\n", file = fileName, append = T)
            cat("amp = ", sites$amp[iSite], "\n", file = fileName, append = T)
            cat("year day rain maxt mint radn\n", file = fileName, append = T)
            cat("  ()  () (mm) (oC) (oC) (mj/m2)\n", file = fileName, append = T)
   
            write.table(odf, file=fileName,append = T, row.names = F, quote = F, col.names = F)
         }
      }
   }
}

# Clean up 
#for (f in list.files(path=cache, glob2rx("*.nc"))) {
#   fcastDay <- strsplit(f, "_")[[1]][4]
#   if (!fcastDay %in% forecastDays) {
#      cat("rm ", paste0(cache,f), "\n")
#      #file.remove(paste0(cache,f))
#   }
#}

for (fcastDay in unique(sapply( 
   list.files(path=cache, glob2rx("*.RData")), function(f) { substr(f,1,8) }))) {
   if (!fcastDay %in% forecastDays) {
      for (g in list.files(path=cache, paste0("^.*",fcastDay))) {
         cat("rm ", paste0(cache,g), "\n")
         file.remove(paste0(cache,g))
      }
   }
}

cat (paste(forecastDays, collapse=","), file="forecastDays.txt")

cat("Finished at ", format.POSIXct(Sys.time()), "\n")
