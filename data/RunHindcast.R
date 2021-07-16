rm(list=ls())
setwd("/home/data/access-s/met/")

live.cache <- "./cache/bom/"          # live forecasts, and silo (observed)
hindcast.cache <- "./cache/hindcast/" # hindcast met files

library("xml2")
library("data.table")

sites<-read.csv("EarlySowingSites.csv")$Name
sowDates <- c("15-jun", "1-jul", "15-jul", "1-aug", "15-aug")

xmlAdd <- function(parent, name) {
  xml_add_child(parent, name)
  return(xml_child(parent, xml_length(parent)))
}

xmlSetNode <- function (parent, node) {
  xml_replace(xml_find_all(parent, paste0("./", xml_name(node))), node)
}

xmlSetText <- function (parent, nodeName, value) {
  xml_remove(xml_find_all(parent, paste0("./", nodeName)))
  xml_add_child(parent, nodeName, value)
}

getNodes <- function (d, xp) {
  nodes<- xml_find_all(d, xp)
  if(length(nodes) == 0) {stop(paste("No nodes matched", xp))}
  return (nodes)
}
if (file.exists("Apsim.simulations")) { file.remove("Apsim.simulations") }

read.met <- function(met.name) {
  header<-readLines(met.name, n=25)
  i<-grep("^year*|^Date*", trimws(header), ignore.case=T)
  if (i <= 0) {return(NULL)}
  met <- read.table(met.name,skip=i+2,na.strings=c("NA","?"))
  names(met) <- unlist(strsplit(trimws(header[i]), " +"))
  return(met)
}

doc <- read_xml("Template_hindcast.apsim")
simulation <- getNodes(doc, "//simulation")[[1]]
name<- xml_attr(simulation, "name")

cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<folder version=\"37\" creator=\"Apsim 7.9-r4045\" name=\"simulations\">\n", file="Hindcast.apsim")

sequenceNumber<- 0; simulations <- vector()
for (site in sites) {
  for(sowdate in sowDates) {
    met<-read.met(paste0(live.cache, site, ".met"))
    met$dm<-sub("^0", "", tolower(format.Date(as.Date(paste(met$year, met$day),"%Y %j"), format="%d-%b")))
    startingConditions<-met[which(sowdate == met$dm),]
    
    for (ens.member in c("obs", paste("e",sprintf("%02d", 1:33),sep=""))) {
      simname <- paste(name, sequenceNumber, sep = "_")
      
      # make a copy to modify
      doc2 <- read_xml("Template_hindcast.apsim")
      simulation <- getNodes(doc2, "//simulation")[[1]]
      xml_attr(simulation, "name") <- simname

      for (x in getNodes(doc2, "//emember")) {
        xml_text(x) <- ens.member
      }
      for (x in getNodes(doc2, "//start_date")) {
        xml_text(x) <- "1/1/1990"
      }
      for (x in getNodes(doc2, "//end_date")) {
        xml_text(x) <- "31/12/2012"
      }
      
      for (x in getNodes(doc2, "//metfile/filename")) {
        xml_text(x) <- paste(live.cache, site, ".met", sep = "")
      }
      
      if (ens.member != "obs") {
        for (x in getNodes(doc2, "//patchinput/filename")) {
          xml_text(x) <-
            paste(hindcast.cache, site, ".", ens.member, ".", sowdate, ".patch.met", sep = "")
        }
      } else {
        for (x in getNodes(doc2, "//patchinput")) {
          xml_remove(x)
        }
        # //dates for hindcast are set in .apsim file ( 1/3/1990 -> 31/12/2012 )
      }
      if (ens.member != "obs") {
        
        # Get the starting conditions for each
        newMet <- xml_find_all(doc2, "//manager[@name='Resets']/script/event[text()='prenewmet']/../text")[[1]]
        for (iyear in 1:nrow(startingConditions)) {
          tmp<- paste0("if (today = date('",sowdate, "-", startingConditions[iyear, "year"], "')) then\n",
                     " met.maxt = ", startingConditions[iyear, "maxt"], "\n",
                     " met.mint = ", startingConditions[iyear, "mint"], "\n",
                     " met.rain = ", startingConditions[iyear, "rain"], "\n",
                     " met.radn = ", startingConditions[iyear, "radn"], "\n",
                     "endif\n")
          xml_text(newMet) <- paste(xml_text(newMet), tmp, sep="\n")
        }
      }
      for (x in getNodes(doc2, "//ui/sowdate")) {
        xml_text(x) <-sowdate
      }
      
      for (x in getNodes(doc2, "//ui/site")) {
        xml_text(x) <- as.character(site)
      }
      
      for (x in getNodes(doc2, "//outputfile/filename")) {
        xml_text(x) <-
          gsub(name, paste(name, ens.member, sep = "_"), xml_text(x), fixed = T)
      }
      
      for (x in getNodes(doc2, "//outputfile/title")) {
        xml_text(x) <-
          gsub(name, paste(name, ens.member, sep = "_"), xml_text(x), fixed = T)
      }
    
      cat(as.character(simulation),"\n", file = "Hindcast.apsim", append = T)
      simulations <- c(simulations, simname)
      sequenceNumber <- sequenceNumber + 1
    }
  }
}

cat("</folder>", file="Hindcast.apsim",append=T)

system2("Apsim.exe", args = "Hindcast.apsim")

# Generic function to read apsim output files. 
read.apsim <- function(apsim.name) {
  header<-readLines(apsim.name, n=25)
  if (length(header) == 0 ) {return(NULL)}
  i<-3  # 4th line
  #  apsim <- read.table(apsim.name,skip=i+1,na.strings=c("NA","?"))
  apsim <- fread(apsim.name,skip=i+1,na.strings=c("NA","?"))
  names(apsim) <- unlist(strsplit(trimws(header[i]), " +"))
  #  cat(apsim.name, " = ", ncol(apsim), "\n")
  return(apsim)
}
df.hindcast<- do.call(rbind, lapply(paste0(simulations, " Daily.out"), read.apsim ))

save(df.hindcast, sowDates, file="Hindcast.RData")

for (f in list.files(path=".", pattern=glob2rx("Hindcast_*.*"))) {
  file.remove(f)
}
