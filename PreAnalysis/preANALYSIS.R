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

#download a limited GTD we created for this assignment
PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/PreAnalysis/pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(PreGTD_in_Memory,'Pre.GTD.csv')
rm(PreGTD_in_Memory)

#Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("Pre.GTD.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]
PreGTD$X <- NULL

# make numeric what we need
PreGTD$EN.URB.LCTY.UR.ZS <- as.numeric(PreGTD$EN.URB.LCTY.UR.ZS)
PreGTD$SP.URB.TOTL <- as.numeric(PreGTD$SP.URB.TOTL)
PreGTD$SP.RUR.TOTL <- as.numeric(PreGTD$SP.RUR.TOTL)
PreGTD$MAX.URB.TOTL <- as.numeric(PreGTD$MAX.URB.TOTL)
PreGTD$pop <- as.numeric(PreGTD$pop)

# some simple new variables, most interesting the relative.city.population_to.largest.city in country!
RegGTD <- data.frame(PreGTD$eventid, PreGTD$iyear)
RegGTD["year"] <- as.numeric(PreGTD$iyear-1970)
RegGTD["city.population_2013"] <- PreGTD$pop
RegGTD["city.population_at.attack.time_liniear.grown"] <- ((PreGTD$pop)*(PreGTD$SP.URB.TOTL)/(PreGTD$MAX.URB.TOTL))
RegGTD["relative.city.population_to.largest.city" ] <- (PreGTD$pop*PreGTD$SP.URB.TOTL/PreGTD$MAX.URB.TOTL/
                                                          ((PreGTD$SP.URB.TOTL+PreGTD$SP.RUR.TOTL)*(100/PreGTD$EN.URB.LCTY.UR.ZS))

# test für eine lineare regression
Linear1 <- lm((relative.city.population_to.largest.city*100) ~ year, data = RegGTD)
summary(Linear1)
plot(Linear1, which = 2)
