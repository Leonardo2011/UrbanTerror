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
PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/TerrorData/Pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(PreGTD_in_Memory,'Pre.GTD.csv')
rm(PreGTD_in_Memory)

#Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("Pre.GTD.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]

# make numeric what we need
PreGTD$EN.URB.LCTY.UR.ZS <- as.numeric(PreGTD$EN.URB.LCTY.UR.ZS)
PreGTD$SP.URB.TOTL <- as.numeric(PreGTD$SP.URB.TOTL)
PreGTD$SP.RUR.TOTL <- as.numeric(PreGTD$SP.RUR.TOTL)
PreGTD$MAX.URB.TOTL <- as.numeric(PreGTD$MAX.URB.TOTL)
PreGTD$pop <- as.numeric(PreGTD$pop)

# take a look at the structure of our data
str(PreGTD)

# make a table of attacks in capital cities per year
with(PreGTD, table(iyear, capital))
table(PreGTD$iyear, PreGTD$capital)
with(PreGTD, table(iyear, (capital))) -> cap.table
summary(cap.table)


# some plots
mosaicplot(cap.table, main="Attacks in Capital Cities", xlab="Year", ylab="Capital Cities (Sum)")
mosaicplot(cap.table, sort=c(2,1))
barplot(cap.table, legend=T, main="Attacks in Capital Cities")
plot(cap.table)


# merge in a coastal city vector
