library("XML")
library("lubridate")

# Helper function to pad a day or month out to 2 digits when applicable
pad <- function(num) {
  return(sprintf("%02d", num))
}

# Expects a .met file with name YYYYMMDD.Site.emember#.met and a .apsim template
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
    file.copy(from = templateFileName, to = outFN)
    doc <- xmlParse(outFN)
    for (node in getNodeSet(doc, "//filename[@name='filename' and @input='yes']")) {
      xmlValue(node) <- paste(date, site, emember, "met", sep=".")
    }
    for (node in getNodeSet(doc, "//clock/start_date")) {
      x <- ymd(date) + days(10)
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
    
    saveXML(doc, outFN)
    return(TRUE)
  }
}

for (filename in list.files(pattern = "*.met")) {
  apsimFileGen(filename, "Template.apsim")
}
