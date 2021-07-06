library("XML")
for (filename in list.files("Example_Met_Files")) {
  
}

sites <- read.csv("wrp/progress/Allsites.csv")


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
      xmlValue(node) <- outFN
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
    
    
    
    saveXML(doc, outFN)
    return(TRUE)
  }
}

