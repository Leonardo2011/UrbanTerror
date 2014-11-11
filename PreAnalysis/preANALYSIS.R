ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("foreign", "car", "RCurl", "ggplot2", "WDI", "httr", "iterators", "dplyr", "plyr",
              "XML", "maps", "ggmap", "Imap", "geosphere", "maptools", "rgeos", "foreach")
ipak(packages)
rm(packages)
rm(ipak)

#download a limited GTD we created for this assignment (5MB instead of 100MB file)
PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/PreAnalysis/pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(PreGTD_in_Memory,'Pre.GTD.csv')
rm(PreGTD_in_Memory)

#Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("Pre.GTD.csv", header=TRUE)
PreGTD<-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]
