rm(list=ls())

setwd("/home/data/access-s/met/")

# set the longitude and latitude of your station
sites<-read.csv("EarlySowingSites.csv", stringsAsFactors = F)

library("ncdf4")
library(parallel)

# Helpers
# convert w/m2 -> mj/m2
w2m<-function(x) {x * (24 * 60 * 60)/1000000}

# Function to read one chunk of data from the opendap server
doit <- function(variable, path) {
  cachedFile<-paste0("./cache/", path)
  if (!file.exists(cachedFile) ) {
    if (!dir.exists(dirname(cachedFile))) 
       dir.create(dirname(cachedFile), recursive=T)
    cat("copying ") ##, path, " to ", cachedFile, " ")
    ret <- 1; errcnt <- 0
    while (ret != 0) {
       ret<- system(paste0("scp pv3579@gadi-dm.nci.org.au:/" , path, " ", cachedFile))
       if ( ret != 0 ) {
          cat("Retrying")
          Sys.sleep(5)
          errcnt <- errcnt + 1 
          if (errcnt > 10) {stop("Error downloading ", cachedFile)}
       }
    }
  }  
  if (!file.exists(cachedFile) ) {
    return(NULL)
  }
  file <-  nc_open(cachedFile)
  daysSince <- ncvar_get(file,"time")
  if (any(daysSince < 0)) {
     daysSince <- 0:(length(daysSince)-1)
     cat("Holey time axis in ", url, "\n")
  }
  myTime<-as.Date(substr(ncatt_get(file, "time")$units, 12, 21)) + daysSince
  nc_close(file)
  
  clusterExport(cl, c( "variable", "myTime", "cachedFile", "w2m" ), environment() )
  clusterEvalQ(cl, file <- nc_open(cachedFile))
  out<-data.frame(date=myTime)
  if (variable != "rsds") {
    out<-cbind(out, do.call(cbind, parLapply(cl, 1:nrow(sites), 
          function(i) {return(as.vector(ncvar_get(file, variable,  
                                                  c(sites$ilon[i], sites$ilat[i],1), 
                                                  c(1,1,length(myTime)))))})))
  } else {
    out<-cbind(out, 
         do.call(cbind, 
         parLapply(cl, 1:nrow(sites), 
         function(i) {return(w2m(as.vector(
           ncvar_get(file, variable,  c(sites$ilon[i], sites$ilat[i],1), 
                     c(1,1,length(myTime))))))})))
  }
  names(out)<-c("date",as.character(sites$Name))
  clusterEvalQ(cl, nc_close(file))
  
  return(out)
}

# Trim
trimIt <- function(chunk, month) {
  if (!is.null(chunk)) {
     y0<-as.Date(paste0(month, "-", format.Date(chunk$date[1], "%Y")), format="%d-%b-%Y")
     chunk<-chunk[chunk$date >= y0,]
  }
  return(chunk)
}

patchFileExists <- function(site, eSequence, month) {
  outfile<-paste0("cache/hindcast/", site, ".e",sprintf("%02d", eSequence), ".", month, ".patch.met")
  return(file.exists(outfile))  
}

writeIt <- function(site) {
  date<-df[["pr"]][,"date"]
  # FIXME add day 366 in leap years
  for (variable in variables) {
    if(length(date) != length(df[[variable]][,site]) ) {
      cat(paste("length", variable, "inconsistent in ", site, "got", length(df[[variable]][,site]), "expected",length(date)))
    }
  }
  
  imonth<- as.numeric(format.Date(date, "%m"))
  
  odf<-cbind(year=format.Date(date, format="%Y"),
             day=as.numeric(format.Date(date, format="%j")),
             rain=round(df[["pr"]][,site],1),
             maxt=round(df[["tasmax"]][,site], 1),
             mint=round(df[["tasmin"]][,site], 1),
             radn=round(df[["rsds"]][,site], 1)) # , vp=round(df[["vprp_09"]][,site], 1)
  
  outfile<-paste0("cache/hindcast/", site, ".e",sprintf("%02d", eSequence), ".", month, ".patch.met")
  cat("# Patched met data\n", file = outfile)
  cat("year day patch_rain patch_maxt patch_mint patch_radn\n", 
      file = outfile, append = T)
  cat("  ()  () (mm) (oC) (oC) (mj/m2)\n", file = outfile, append = T)
  
  write.table(odf, file=outfile,append = T, row.names = F, quote = F, col.names = F)
  
#  met <-read.met(paste0(site,".met"))
#  met<-met[met$date >= as.Date("1990-01-01") & met$date <= as.Date("2013-12-31"), ]
#  idx<- match(as.character(date), as.character(met$date))
#  met$ymd<-format.Date(met$date, "%Y%m%d")
#  met$jd<-sprintf("%5d", as.integer(format.Date(met$date, "%j")))
#  met$rain[idx] <-  df[["pr"]][,site]
#  met$maxt[idx] <-  df[["tasmax"]][,site]
#  met$mint[idx] <-  df[["tasmin"]][,site]
#  met$radn[idx] <-  radn
#  met$pan[idx] <-  df[["evap"]][,site]
#  met$vp[idx] <-  df[["vprp_09"]][,site]
#  outfile<-paste0(site, ".e",sprintf("%02d", eSequence), ".", month, ".p51")
#  cat(sprintf("%6.2f %6.2f evap shifted, rad from oktas\n",
#              attributes(met)$latitude,
#              attributes(met)$longitude), file = outfile)
#  cat("  date    jday  tmax  tmin  rain  evap   rad   vp\n", file = outfile, append = T)
#  contents <- paste(
#     sprintf(fmt=" %s%s%6.1f%6.1f%6.1f%6.1f%6.1f%6.1f", 
#             met$ymd, met$jd, met$maxt, met$mint, met$rain, met$pan, met$radn, met$vp), 
#             collapse = "\n")
#  cat(contents, "\n", file = outfile, append = T)
}

cachedFile<-"cache/g/data/ub7/access-s1/hc/calibrated_5km_v3/atmos/pr/daily/e01/daq5_pr_19911101_e01.nc"
if (!file.exists(cachedFile) ) {
    if (!dir.exists(dirname(cachedFile))) 
       dir.create(dirname(cachedFile), recursive=T)
    system(paste0("scp pv3579@gadi-dm.nci.org.au:/g/data/ub7/access-s1/hc/calibrated_5km_v3/atmos/pr/daily/e01/daq5_pr_19911101_e01.nc ", cachedFile))
}  
file <-  nc_open(cachedFile)
lon <- ncvar_get(file,"lon")
lat <- ncvar_get(file,"lat")
nc_close(file)
sites$ilat <- NA; sites$ion <- NA;
for (i in 1:nrow(sites)) {
  # find the latitude index of the array corresponding to nearest model value
  dlat <- abs(lat-sites$lat[i])
  minv <- min(dlat)
  sites$ilat[i] <- which(dlat==minv)
  
  # find the latitude index of the array corresponding to nearest model value
  dlon <- abs(lon-sites$long[i])
  minv <- min(dlon)
  sites$ilon[i] <- which(dlon==minv)
}

read.met <- function(met.name) {
  hdr<-readLines(met.name, n = 30)
  hdrLine<- which(grepl(pattern = "^year", hdr))
  latitude<-as.numeric(unlist(strsplit(hdr[which(grepl("^latitude",hdr))],split = " +"))[3])
  longitude<-as.numeric(unlist(strsplit(hdr[which(grepl("^longitude",hdr))],split = " +"))[3])
  met <- read.table(met.name,skip=hdrLine+1,na.strings=c("NA","?"))
  names(met) <- unlist(strsplit(hdr[hdrLine], " +"))
  met$date <- as.Date(paste(met$year,met$day),"%Y %j")
  attributes(met)$latitude<-latitude
  attributes(met)$longitude<-longitude
  return(met)
}

is.leapyear=function(year) {
      #http://en.wikipedia.org/wiki/Leap_year
      return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}
   # The forecasts we are interested in. x.y for x=month, y=year offset (december)
 monthDays<-list()
#   monthDays[["1-may"]] <- c("0501", "0401")
monthDays[["1-jun"]] <- c("0517.0", "0525.0", "0601.0")
monthDays[["15-jun"]] <- c("0601.0", "0609.0", "0617.0")
monthDays[["1-jul"]] <- c("0617.0", "0625.0", "0701.0")
monthDays[["15-jul"]] <- c("0701.0", "0709.0", "0717.0")
monthDays[["1-aug"]] <- c("0717.0", "0725.0", "0801.0")
monthDays[["15-aug"]] <- c("0801.0", "0809.0", "0817.0")
monthDays[["1-sep"]] <- c("0817.0", "0825.0", "0901.0")
monthDays[["15-sep"]] <- c("0901.0", "0909.0", "0917.0")
monthDays[["1-oct"]] <- c("0917.0", "0925.0", "1001.0")
monthDays[["15-oct"]] <- c("1001.0", "1009.0", "1017.0")

# monthDays[["15-sep"]] <- c("0901.0", "0909.0", "0917.0" )
# monthDays[["1-oct"]] <- c("0917.0", "0925.0", "1001.0")
# monthDays[["15-oct"]] <- c("1001.0", "1009.0", "1017.0" )
# monthDays[["1-nov"]] <- c("1017.0", "1025.0", "1101.0")
# monthDays[["15-nov"]] <- c("1101.0", "1109.0", "1117.0" )
# monthDays[["1-dec"]] <- c("1117.0", "1125.0", "1201.0")
# monthDays[["15-dec"]] <- c("1201.0", "1209.0", "1217.0" )
# monthDays[["1-jan"]] <- c("1217.-1", "1225.-1", "0101.0")
# monthDays[["15-jan"]] <- c("0101.0", "0109.0", "0117.0" )
 
 variables<-c("pr", "tasmax", "tasmin", "rsds")  # "vprp_09", "evap"
   directoryname<-list();
   directoryname[["pr"]] <- "pr";
   directoryname[["tasmax"]] <- "tasmax";
   directoryname[["tasmin"]] <- "tasmin";
   directoryname[["evap"]] <- "evap"
   directoryname[["vprp_09"]] <- "vprp_09"
   directoryname[["vprp_15"]] <- "vprp_15"
   directoryname[["rsds"]] <- "rsds"
   directoryname[["wind_speed"]] <- "wind_speed"
   
   # Initiate cluster
   cl <- makeCluster(detectCores() - 1)  
   clusterExport(cl, c("sites", "variables", "read.met"))
   clusterEvalQ(cl, library("ncdf4"))
   clusterEvalQ(cl, library("sirad"))

   
   emembers<-c("e01", "e02", "e03", "e04", "e05", "e06", "e07", "e08", "e09", "e10", "e11")
   for (month in names(monthDays)) {
      # the ensemble members for a point in time include previous runs.
      eSequence <- 1
      for (emember in emembers) {
         for (XY in monthDays[[month]]) {
           x<- 0
           for (site in sites$Name) { if (patchFileExists(site, eSequence, month)) { x <- x + 1 } }
           if (x == nrow(sites)) { eSequence <- eSequence + 1; next }
           
           monthDay<- unlist(strsplit(XY, ".", fixed=T))[1]
           yearOffset<- as.numeric(unlist(strsplit(XY, ".", fixed=T))[2])
           df<-list()
           for (variable in variables) { df[[variable]] <- vector("list", 23) }
           # Download and read data
           for (year in 1990:2012) {
               for (variable in variables) {
                  path<-paste("g/data/ub7/access-s1/hc/calibrated_5km_v3/atmos/",
                                directoryname[[variable]], "/daily/", emember, "/daq5_", variable, 
                                "_", year + yearOffset, monthDay, "_",emember, ".nc",sep="")
                  if (year + yearOffset >= 1990) { 

                  chunk <- doit (variable, path)
                  chunk <- trimIt(chunk, month)

                  cat(path, " = ", nrow(chunk), " lines\n")
                  df[[variable]][[year - 1989]] <- chunk
                  }
               }
           }
           for (variable in variables) {
             df[[variable]] <- do.call(rbind, df[[variable]])
           }
           clusterExport(cl, c("df", "eSequence", "month"))

           # Write each site
           parLapply(cl, sites$Name, writeIt )
           eSequence <- eSequence + 1
         }
      }
   }

stopCluster(cl)
   
