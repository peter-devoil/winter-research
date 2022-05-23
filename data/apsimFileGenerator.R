library("XML")
library("lubridate")

print(paste("apsimFileGenerator.R started at", now()))

metFileDir <- "cache/bom/"
outFileDir <- "cache/bom/"

# Helper function to pad a day or month out to 2 digits when applicable
pad <- function(num) {
  return(sprintf("%02d", num))
}

# Returns a list of length 2 containing the first and last dates in the .met file
getDates <- function(metFileName) {
  header <- readLines(metFileName, n = 25L)
  footer <- tail(readLines(metFileName), n = 25L)
  
  # This is error prone, but the first row of data should be 2 lines after col names.
  i <- grep("^year day rain maxt mint radn$", trimws(header)) + 2
  if (i <= 0) {return(NULL)}
  
  startLine <- strsplit(header[i], split = " ")[[1]][1:2]
  start <- parse_date_time(paste(startLine[1], startLine[2]), "Y j")
  
  indices <- grep("[[:digit:]]", footer)
  if (indices) {endLine <- footer[[max(indices)]][[1]]} else {return(NULL)}
  endLine <- strsplit(endLine, split = " ")[[1]]
  end <- parse_date_time(paste(endLine[1], endLine[2]), "Y j")
  
  return(list(start, end))
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
    doc <- xmlParse(templateFileName)
    for (node in getNodeSet(doc, "//filename[@name='filename' and @input='yes']")) {
      xmlValue(node) <- paste(date, site, emember, "met", sep=".")
    }
    
    for (node in getNodeSet(doc, "//simulation")) { # Set simulation name
      removeAttributes(node)
      xmlAttrs(node) <- c(name = paste(date, site, emember, sep = "_"))
    }
    
    # get Dates from the contents of the metfile, rather than the name of the metfile
    dates <- getDates(paste0(metFileDir, metFileName))
    if (length(dates) != 2) {
      stop("Can't read start and end dates from metfile: ", metFileName)
    }
    
    for (node in getNodeSet(doc, "//clock/start_date")) {
      x <- dates[[1]]
      xmlValue(node) <- paste0(pad(day(x)), '/', pad(month(x)), '/', year(x))
    }
    for (node in getNodeSet(doc, "//clock/end_date")) {
      x <- dates[[2]]
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
    
    saveXML(doc, paste0(outFileDir, outFN))
    return(TRUE)
  }
}

# Should match filenames of the format "YYYYMMDD.sitename.eXX.out".
for (filename in list.files(metFileDir, pattern = "[[:digit:]]{8}[.]\\w+[.]e[[:digit:]]{2}[.]met$")) {
  apsimFileGen(filename, "Template.apsim")
}

print(paste("apsimFileGenerator.R finished at", now()))
