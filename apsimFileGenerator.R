library("XML")
library("lubridate")

# Helper function to pad a day or month out to 2 digits when applicable
pad <- function(num) {
  return(sprintf("%02d", num))
}

# Expects a .met file with name YYYYMMDD.sitename.eXX.met and a .apsim template
# Will do nothing if an apsim file with the same name as metfile is already there. 
apsimFileGen <- function(metFileName, templateFileName) {

  dummy <- strsplit(metFileName, "\\.")[[1]]
  date <- dummy[[1]]
  site <- dummy[[2]]
  emember <- dummy[[3]]
  outFN <- paste(date, site, emember, "apsim", sep=".")
  
  # assumes the apsim files are in current working directory by getwd()
  if (outFN %in% list.files()) { # skiip if there's already an apsim file
    return(TRUE)
  } else {
    #file.copy(from = templateFileName, to = outFN)
    doc <- xmlParse(templateFileName)
    for (node in getNodeSet(doc, "//filename[@name='filename' and @input='yes']")) {
      xmlValue(node) <- paste(date, site, emember, "met", sep=".")
    }
    
    for (node in getNodeSet(doc, "//simulation")) { # Set simulation name
      removeAttributes(node)
      xmlAttrs(node) <- c(name = paste(date, site, emember, sep = "_"))
    }
    for (node in getNodeSet(doc, "//clock/start_date")) {
      x <- ymd(date) + days(10) #FIXME get dates from met file
      xmlValue(node) <- paste0(pad(day(x)), '/', pad(month(x)), '/', year(x))
    }
    for (node in getNodeSet(doc, "//clock/end_date")) {
      x <- ymd(date) + days(60)
      xmlValue(node) <- paste0(pad(day(x)), '/', pad(month(x)), '/', year(x))
    }
    for (node in getNodeSet(doc, "//filename[@output='yes']")) {
      xmlValue(node) <- paste(date, site, emember, "out", sep=".")
    }
    for (node in getNodeSet(doc, "//ui/site")) {
      xmlValue(node) <- site
    }
    for (node in getNodeSet(doc, "//ui/emember")) {
      xmlValue(node) <- emember
    }
    for (node in getNodeSet(doc, "//outputfile/filename")) {
      xmlValue(node) <- paste(date, site, emember, "out", sep=".")
    }
    for (node in getNodeSet(doc, "//outputfile/title")) {
      xmlValue(node) <- paste(date, site, emember, sep=".")
    }
    
    saveXML(doc, paste0("files/", outFN))
    return(TRUE)
  }
}

# Should match filenames of the format "YYYYMMDD.sitename.eXX.out".
# FIXME why can't I prepend the below pattern with '^'? Confused. 
for (filename in list.files("files", pattern = "[[:digit:]]{6}[.]\\w+[.]e[[:digit:]]{2}[.]met$")) {
  apsimFileGen(filename, "Template.apsim")
}
