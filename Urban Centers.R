
#############################
###building a city dataset###
#############################

install.packages("httr")
install.packages("dplyr")
install.packages("XML")


###############################
######## BIG CITY DATA ########
###############################


worldcitiespop <- read.csv("worldcitiespop.txt")

### cities with a known population with more than 333000 inhabitants
C620big <- subset(worldcitiespop, select = c(Country, City, AccentCity, Region, Latitude, Longitude, Population), Population > 10000)



###############################
##### URBAN CENTERS DATA ######
###############################

# Scrap Wiki on urban Centers

library(httr)
library(dplyr)
library(XML)

URL <- 'http://en.wikipedia.org/w/index.php?title=List_of_urban_areas_by_population&section=2'

table <- readHTMLTable(URL)
UrbanCenters <- table [[2]] 
UrbanCenters$City <- gsub("\\[.+?\\]","", UrbanCenters$City)
UrbanCenters$City <- gsub("\\(.+?\\)","", UrbanCenters$City)
UrbanCenters$City <- gsub("[[:digit:]]", "", UrbanCenters$City)
UrbanCenters$City <- gsub("[[:punct:]]", "", UrbanCenters$City)

UrbanCenters["AccentCity"] <- UrbanCenters$City






#### try to merge, not leading anywhere but if city names in UrbanCenters are cleaned, it could lead somewhere
try <- merge(C620big, UrbanCenters, by=c("AccentCity"))
