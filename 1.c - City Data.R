# MPP-E1180: Introduction to Collaborative Social Science Data Analysis
### Fall 2014
### Instructor: Christopher Gandrud

##############################################
############## URBAN TERROR ##################
##############################################
###############  Part 1: DATA   ##############
##############################################
##########Lukas B Cameron R Sascha S##########
##############################################
############# City Level Data ################
##############################################



#############################################################################################################
######################################### 1. Urban Centers ##################################################
#############################################################################################################


if(file.exists("Cache/UrbanCenters.csv"))
{UrbanCenters <- read.csv("Cache/UrbanCenters.csv")
} else
{
  
  ##### Urban Centers #####
  
  #We use a Wikipedia article on global Urban Centers for our analysis on the geographical distance from the location of attacks
  #to the nearest, "important" city. We then use the google.maps API to geolocate each of the centers.
  
  
  ###### Gathering Data  ######
  
  # Scrap Wiki on Urban Centers using the 'XML' package
  URL <- 'http://en.wikipedia.org/w/index.php?title=List_of_urban_areas_by_population&section=2'
  table <- readHTMLTable(URL, encoding = "UTF-16")
  UrbanCenters <- table [[2]]
  UrbanCenters$City <- as.character(UrbanCenters$City)
  colnames(UrbanCenters)[6] <- "Area"
  colnames(UrbanCenters)[7] <- "Density"
  
  
  ##### Cleaning Data #####
  
  #Cleaning the Urban Centers name in order to allow google.maps API to find them
  UrbanCenters$City <- gsub("\\xc3\xa1","a", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xa2","a", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xa3","a", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xa4","a", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xa5","a", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xa0","a", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xa8","e", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xa9","e", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xaa","e", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xab","e", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xb1","n", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xb2","o", perl=TRUE, UrbanCenters$City) 
  UrbanCenters$City <- gsub("\\xc3\xb3","o", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xb4","o", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xb5","o", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xb6","o", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xb9","u", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xba","u", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xbb","u", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xbc","u", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc5\xab","u", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xac","i", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xad","i", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xae","i", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\xaf","i", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc4\xaf","i", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x80","A", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x81","A", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x82","A", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x83","A", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x84","A", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x85","A", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x88","E", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x89","E", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x8a","E", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x8b","E", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x8c","I", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x8d","I", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x8e","I", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x8f","I", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc4\xb0","I", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x91","N", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x92","O", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x93","O", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x94","O", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x95","O", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x98","O", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x99","U", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x9a","U", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x9c","U", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xc3\x9b","U", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\(.*","", UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xe2\x80\x91.*","", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xe2\x80\x92.*","", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xe2\x80\x93.*","", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\xe2\x80\x94.*","", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\,.*","", UrbanCenters$City)
  UrbanCenters$City <- gsub("\\[.+?\\]","", UrbanCenters$City)
  UrbanCenters$City <- gsub("\\(.+?\\)","", UrbanCenters$City)
  #UrbanCenters$City <- gsub("[[:digit:]]", "", UrbanCenters$City)
  UrbanCenters$City <- gsub("\\x2d.*","", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("Region", "", UrbanCenters$City)
  UrbanCenters$City <- gsub("Greater ", "", UrbanCenters$City)
  UrbanCenters$City <- gsub(" de la Sierra", "", UrbanCenters$City)
  UrbanCenters$City <- gsub("\\x2d","", perl=TRUE, UrbanCenters$City)
  UrbanCenters$City <- gsub("\\.","",UrbanCenters$City)
  UrbanCenters$City <- gsub("\\\\", "", UrbanCenters$City)
  UrbanCenters$City <- gsub("\\(","",UrbanCenters$City)
  UrbanCenters$City <- gsub("\\)","",UrbanCenters$City)
  
  UrbanCenters$Country <- gsub("^..", "", UrbanCenters$Country)
  UrbanCenters$Country[UrbanCenters$City=="Kathmandu"] <- "Nepal"
  CoCi <-data.frame(paste(UrbanCenters$Country, UrbanCenters$City, sep=", "), row.names = NULL)
  
  UrbanCenters$City <- gsub("\\,","",UrbanCenters$City)
  UrbanCenters$City <- gsub("\\-","",UrbanCenters$City)
  UrbanCenters$City <- gsub("\\'","",UrbanCenters$City)
  UrbanCenters$City <- gsub("\\'","",UrbanCenters$City)
  UrbanCenters$City <- gsub("\\`","",UrbanCenters$City)
  UrbanCenters$City <- gsub("\\'","",UrbanCenters$City)
  UrbanCenters$City <- gsub(" ","",UrbanCenters$City)
  UrbanCenters$City <- tolower(UrbanCenters$City)
  
  
  ###### Manipulate Data ######
  
  #Putting in a string with "Country, City" to allow google.maps API to find them
  CoCi["loc"] <- CoCi
  CoCi$loc <- as.character(CoCi$loc)
  CoCiLoc<-(CoCi$loc)
  
  #Looking up lon/lat data for each Urban Center via google.maps using the geocode function of the package maps.
  #This is a very time consuming process, which is why we chose to cache the result to reduce computation time for this script.
  if(file.exists("Cache/UrbanLoc.csv"))
  {UrbanLoc <- read.csv("Cache/UrbanLoc.csv")
  } else
  {
    UrbanLoc <- geocode(CoCiLoc, output = c("latlon", "latlona", "more", "all"),messaging = FALSE, sensor = FALSE, override_limit = TRUE)
    write.csv(UrbanLoc, "Cache/UrbanLoc.csv")
  }
  
  #Inlcuding geographic data into the original Urban Centers data frame
  UrbanCenters["lat"] <- UrbanLoc$lat
  UrbanCenters["lon"] <- UrbanLoc$lon
  UrbanCenters["full.name"] <- CoCiLoc
  UrbanCenters$City <- tolower(UrbanCenters$City)
  
  # remove commas
  UrbanCenters$Population <- gsub("\\,","",UrbanCenters$Population)
  UrbanCenters$Area <- gsub("\\,","",UrbanCenters$Area)
  UrbanCenters$Density <- gsub("\\,","",UrbanCenters$Density)
  
  # Assigning a 1 to all the largest urban centers in a country
  UrbanCenters$Population <- as.numeric(UrbanCenters$Population)
  LUC <- aggregate(Population ~ Country,UrbanCenters,max)
  LUC["largest.UC"] <- 1
  UrbanCenters <- merge(UrbanCenters, LUC, by=c("Population", "Country"), all.x=TRUE)
  UrbanCenters <- UrbanCenters[order(-UrbanCenters$Population), ]
  UrbanCenters$largest.UC[is.na(UrbanCenters$largest.UC)] <-0
  rm(LUC)
  
  
  #Including dummy variables for coastal megacities
  source('SmallScripts/CoastalMC.R')
  
  #Deleting what is not needed anymore
  rm(UrbanLoc, table ,URL, CoCi, CoCiLoc)
  
  #Caching the Urban Centers dataframe
  write.csv(UrbanCenters, "Cache/UrbanCenters.csv")
  
}


#############################################################################################################
###################################### 2. Three City Data Sets ###############################################
#############################################################################################################

if(file.exists("Cache/world.cities.csv"))
{world.cities <- read.csv("Cache/world.cities.csv")
} else
{

    
  ############## City Data  ##############
  
  #The purpose of this script is to gather, clean, and merge three different datasets on cities of the world with similar information.
  #We merge them to increase the total number of cities that could match the GTD.
  
  
  ###### Gathering Data  ######
    
  ###World cities dataset 1/3 (worldcities2013)###
  
  # it stems from a private company, Maxmind Inc.
  #http://download.maxmind.com/download/worldcities/worldcitiespop.txt.gz and transformed into CSV, since it is not available
  #in .csv format right away.
  unzip("Downloaded_Data/worldcitiespop1000.zip", exdir="Downloaded_Data")
  worldcities2013 <- read.csv("Downloaded_Data/worldcitiespop.csv")
  
  
  ###World cities dataset 2/3 (worldcities1000)###
  
  #http://download.geonames.org/export/dump/cities1000.zip and transformed into CSV, since it is not available
  #in .csv format right away. The GeoNames geographical database is available for download free of charge under 
  #a creative commons attribution license.
  
  worldcities1000 <- read.csv("Downloaded_Data/worldcitiespop1000.csv") 
  
  # the first both city data sets are very similar, this brought together right away as worldcities2013
  worldcities1000 <- subset(worldcities1000, select=c("V9", "V3", "V2", "V18", "V5", "V6", "V15"), !is.na(V9) &!is.na(V5) & V15!=0)
  worldcities1000$V3 <- tolower(worldcities1000$V3)
  worldcities1000$V9 <- tolower(worldcities1000$V9)
  worldcities2013 <- merge(worldcities2013, worldcities1000, by.x=c("Country", "City", "AccentCity", "Region", "Latitude", "Longitude", "Population"), 
                           by.y=c("V9", "V3", "V2", "V18", "V5", "V6", "V15"), all=TRUE)
  rm(worldcities1000)
  worldcities2013 <- worldcities2013[order(-worldcities2013$Population),]
  worldcities2013["merge"] <- paste(worldcities2013$Country, worldcities2013$City, sep="xxx")
  worldcities2013 <- worldcities2013[!duplicated(worldcities2013$merge),]
  worldcities2013$merge <- NULL  
  

  ###### Cleaning Data  ######
  
  #Our standard for city names is lowercase letters with no special characters from the worldcities2009 dataset 
  #for easier merging purposes, and lowercase country names (no special charackters) from the world.cities2013 dataset.
  
  ###World cities dataset 1/2 (worldcities2013)###
  
  #introduce Akkaraipattu as it was missing in the original dataset
  worldcities2013 <- rbind(worldcities2013, data.frame(X=0,Country="lk", City="Akkaraipattu", AccentCity="Akkaraipattu", 
                                                       Region= 31, Latitude=7.227862, Longitude=81.850551,Population=35000))
  worldcities2013 <- rbind(worldcities2013, data.frame(X=0,Country="pk", City="Bara", AccentCity="Bara", 
                                                       Region= 31, Latitude=33.916609, Longitude=71.462714,Population=30000))
  
  
  # Change Country Code to Countries on the level of the other set
  source('SmallScripts/CountryCode.R')
  worldcities2013$Country <- tolower(worldcities2013$Country)
  
  #Recoding city names to our standard
  worldcities2013$City <- gsub(" ", "", worldcities2013$City)
  worldcities2013$City <- tolower(worldcities2013$City)
  
  
  
  
  ###World cities dataset 3/3 (world.cities from the 'maps' package)###
  
  #The dataset is called world.cities. Since it is from 2009, we call it worldcities2009.
  data(world.cities)
  worldcities2009 <- world.cities
  rm(world.cities)

  
  #the dataset has missing or wrong information
  worldcities2009$capital[worldcities2009$name == "delhi" & worldcities2009$country.etc == "India"] <- "1"
  worldcities2009$name[worldcities2009$name == "soul" & worldcities2009$country.etc == "Korea South"] <- "seoul"
  worldcities2009$name[worldcities2009$name == "bombay" &  worldcities2009$country.etc  == "India"] <- "mumbai"
  worldcities2009$name[worldcities2009$name == "new york" &  worldcities2009$country.etc  == "USA"] <- "new york city"
  
  #Reaching/Defining country and city standard
  worldcities2009$country.etc[worldcities2009$country.etc == "Russia"] <- "Russian Federation"
  worldcities2009$country.etc[worldcities2009$country.etc == "UK"] <- "United Kingdom"
  worldcities2009$country.etc[worldcities2009$country.etc == "USA"] <- "United States of America"
  worldcities2009$country.etc[worldcities2009$country.etc == "Korea North"] <- "Korea, Democratic People's Republic of"
  worldcities2009$country.etc[worldcities2009$country.etc == "Korea South"] <- "Korea, Republic of"
  worldcities2009$country.etc[worldcities2009$country.etc == "Sicily"] <- "Italy"
  worldcities2009$country.etc[worldcities2009$country.etc == "East Timor"] <- "Timor-Leste"
  worldcities2009$country.etc[worldcities2009$country.etc == "Madeira"] <- "Portugal"
  worldcities2009$country.etc[worldcities2009$country.etc == "Madiera"] <- "Portugal"
  worldcities2009$country.etc[worldcities2009$country.etc == "Cape Verde"] <- "caboverde"
  worldcities2009$country.etc<-gsub(" ", "",worldcities2009$country.etc, ignore.case=TRUE)
  worldcities2009$country.etc<-gsub("\\,", "",worldcities2009$country.etc, ignore.case=TRUE)
  worldcities2009$country.etc <- tolower(worldcities2009$country.etc)
  worldcities2009$name <- gsub(" ", "", worldcities2009$name)
  worldcities2009$name <- tolower(worldcities2009$name)
  
  
  ###### Manipulate Data  ######
  
  #We now merge the three datasets into one. First, some preparation.
  
  #Ordering to later enable selection on duplicates
  worldcities2013 <- worldcities2013[order(-worldcities2013$Population, na.last=TRUE) , ]
  worldcities2009 <- worldcities2009[order(-worldcities2009$pop, na.last=TRUE) , ]
  
  #Creating a subset with matching columns
  worldcities2013 <- subset(worldcities2013, select =c("City", "Country", "Population", "Latitude", "Longitude", "Region"))
  colnames(worldcities2013)[1] <- "name"
  colnames(worldcities2013)[2] <- "country.etc"
  colnames(worldcities2013)[3] <- "pop"
  colnames(worldcities2013)[4] <- "lat"
  colnames(worldcities2013)[5] <- "long"
  
  #Creating a column to merge over: countrycity
  worldcities2009$merge <- paste(worldcities2009$country.etc, worldcities2009$name, sep="")
  worldcities2013$merge <- paste(worldcities2013$country.etc, worldcities2013$name, sep="")
  
  #Merging to new set: "world.cities"
  world.cities <- merge(worldcities2009, worldcities2013, by= c("merge", "name", "country.etc", "pop", "lat", "long"), all=TRUE)
  world.cities <- world.cities[order(world.cities$merge, world.cities$capital, world.cities$pop),]
  world.cities <- world.cities[!duplicated(world.cities$merge), ]
  
  world.cities <- world.cities[order(-world.cities$pop), ]
  rm(worldcities2013, worldcities2009)
  
  
  
  
  #Bringing country names in combined world.cities to WDI lowercase standard
  source('SmallScripts/CountryCleaninginCitySet.R')
  X <- world.cities$country.etc
  source('SmallScripts/CleanSpecialCharacters.R')
  world.cities$country.etc <- X
  rm(X)

  
  
  #create cache world.cities.csv
  write.csv(world.cities, "Cache/world.cities.csv")
  
  unlink("Downloaded_Data/worldcitiespop1000.csv")
  unlink("Downloaded_Data/worldcitiespop.csv")
  
  
}



#############################################################################################################
####################### 3. Merging Data: Both City Data Sets and Urban Centers ###############################
#############################################################################################################


#In this script we merge the UrbanCenters and world.cities databases so that each 
#city is allocated to its closest Urban Center (including distances)

# times 4: we splitted the city dataset into 4 sets to make this operation work on machines with less RAM...
world.citiesa <- subset (world.cities, pop<=median(pop))
world.citiesb <- subset (world.cities, !pop<=median(pop))
world.cities1 <- subset (world.citiesa, !pop<=median(pop))
world.cities2 <- subset (world.citiesa, pop<=median(pop))
world.cities3 <- subset (world.citiesb, !pop<=median(pop))
world.cities4 <- subset (world.citiesb, pop<=median(pop))
rm(world.citiesa, world.citiesb)

#Renaming columns and select subsets for merging over fake variable to create a matrix 
#with each city and each Urban Center for each city (~ 120.000 Cities X ~ 500 urban Centers) 
UCmerge <- subset(UrbanCenters, select = c("lon", "lat", "full.name"))
DTUC <- data.table(UCmerge)
DTUC[,fake:=1]
WCmerge1 <-subset(world.cities1, select = c("long", "lat"))
WCmerge1["CityID"] <- rownames(world.cities1)
DTWC1 <- data.table(WCmerge1)
DTWC1[,fake:=1]
ALLDIST1 <-merge(DTUC, DTWC1, by='fake',allow.cartesian=TRUE)
rm(WCmerge1, DTWC1)


WCmerge2 <-subset(world.cities2, select = c("long", "lat"))
WCmerge2["CityID"] <- rownames(world.cities2)
DTWC2 <- data.table(WCmerge2)
DTWC2[,fake:=1]
ALLDIST2 <-merge(DTUC, DTWC2, by='fake',allow.cartesian=TRUE)
rm(WCmerge2, DTWC2)
WCmerge3 <-subset(world.cities3, select = c("long", "lat"))
WCmerge3["CityID"] <- rownames(world.cities3)
DTWC3 <- data.table(WCmerge3)
DTWC3[,fake:=1]
ALLDIST3 <-merge(DTUC, DTWC3, by='fake',allow.cartesian=TRUE)
rm(WCmerge3, DTWC3)
WCmerge4 <-subset(world.cities4, select = c("long", "lat"))
WCmerge4["CityID"] <- rownames(world.cities4)
DTWC4 <- data.table(WCmerge4)
DTWC4[,fake:=1]
ALLDIST4 <-merge(DTUC, DTWC4, by='fake',allow.cartesian=TRUE)
rm(WCmerge4, DTWC4, UCmerge, DTUC, world.cities1, world.cities2, world.cities3, world.cities4)



#Finding each distance ( ~ 30 million individual distances will be found )
ALLDIST1 <- ALLDIST1[order(ALLDIST1$CityID), ]
ALLDIST1$DISTkm <- (distCosine(matrix(c(ALLDIST1$long, ALLDIST1$lat.y), ncol=2),matrix(c(ALLDIST1$lon, ALLDIST1$lat.x), ncol=2), r=6378137)/1000)
ALLDIST1 <- subset(ALLDIST1, select=c(CityID, DISTkm, full.name))
ALLDIST1 <-  data.table(ALLDIST1)
#Reducing to only the closest Urban Center for each and every city
ALLDIST1.min  <- ALLDIST1[,list(DISTkm = min(DISTkm)),by=list(CityID)]
ALLDIST1.min <- merge(ALLDIST1.min, ALLDIST1, by=c("CityID","DISTkm"), all.y=FALSE)
rm(ALLDIST1)

#We will merge this back into the original City Data set later. Now, we repeat the two steps above
#for the othe rparts of the splittet city data set (all because of the RAM issue)
#2
ALLDIST2$DISTkm <- (distCosine(matrix(c(ALLDIST2$long, ALLDIST2$lat.y), ncol=2),matrix(c(ALLDIST2$lon, ALLDIST2$lat.x), ncol=2), r=6378137)/1000)
ALLDIST2 <- subset(ALLDIST2, select=c(CityID, DISTkm, full.name))
ALLDIST2 <-  data.table(ALLDIST2)
ALLDIST2.min  <- ALLDIST2[,list(DISTkm = min(DISTkm)),by=list(CityID)]
ALLDIST2.min <-merge(ALLDIST2.min, ALLDIST2, by=c("CityID","DISTkm"), all.y=FALSE)
rm(ALLDIST2)

#3
ALLDIST3$DISTkm <- (distCosine(matrix(c(ALLDIST3$long, ALLDIST3$lat.y), ncol=2),matrix(c(ALLDIST3$lon, ALLDIST3$lat.x), ncol=2), r=6378137)/1000)
ALLDIST3 <- subset(ALLDIST3, select=c(CityID, DISTkm, full.name))
ALLDIST3 <-  data.table(ALLDIST3)
ALLDIST3.min  <- ALLDIST3[,list(DISTkm = min(DISTkm)),by=list(CityID)]
ALLDIST3.min <-merge(ALLDIST3.min, ALLDIST3, by=c("CityID","DISTkm"), all.y=FALSE)
rm(ALLDIST3)

#4
ALLDIST4$DISTkm <- (distCosine(matrix(c(ALLDIST4$long, ALLDIST4$lat.y), ncol=2),matrix(c(ALLDIST4$lon, ALLDIST4$lat.x), ncol=2), r=6378137)/1000)
ALLDIST4 <- subset(ALLDIST4, select=c(CityID, DISTkm, full.name))
ALLDIST4 <-  data.table(ALLDIST4)
ALLDIST4.min  <- ALLDIST4[,list(DISTkm = min(DISTkm)),by=list(CityID)]
ALLDIST4.min <-merge(ALLDIST4.min, ALLDIST4, by=c("CityID","DISTkm"), all.y=FALSE)
rm(ALLDIST4)

#Bring them all together again
ALLDIST.min <- merge(ALLDIST1.min, ALLDIST2.min, by=c("CityID", "DISTkm", "full.name"), all=TRUE)
ALLDIST.min <- merge(ALLDIST.min, ALLDIST3.min, by=c("CityID", "DISTkm", "full.name"), all=TRUE)
ALLDIST.min <- merge(ALLDIST.min, ALLDIST4.min, by=c("CityID", "DISTkm", "full.name"), all=TRUE)
rm(ALLDIST1.min, ALLDIST2.min, ALLDIST3.min, ALLDIST4.min)


# merge with world.cities and UrbanCenters 
world.citiesID <- world.cities
world.citiesID["CityID"] <-rownames(world.citiesID)
WC.UC <- merge(world.citiesID, ALLDIST.min, by="CityID", all.x=TRUE)
rm(world.citiesID, ALLDIST.min)
WC.UC <- merge(WC.UC, UrbanCenters, by=c("full.name"), all.x=TRUE)


#Housekeeping
X<-WC.UC$name
source('SmallScripts/CleanSpecialCharacters.R')
WC.UC$name <- X
X<-WC.UC$country.etc
source('SmallScripts/CleanSpecialCharacters.R')
source('SmallScripts/CountryCleaninginCitySet.R')
WC.UC$country.etc <- X
rm(X)
WC.UC["Closest.Urban.Center"] <- WC.UC$full.name
WC.UC["WC.UC.dist.km"] <- WC.UC$DISTkm
WC.UC$WC.UC.dist.km <- as.numeric(WC.UC$WC.UC.dist.km) 
WC.UC$Area <- as.numeric(WC.UC$Area) 
WC.UC$capital <- as.numeric(WC.UC$capital)

#Introducing logical variable that tells us whether a city is part of an Urban Center or close to one.
#We use a calculation that treates the middle of the city as the center of a circle.
WC.UC["part.of.urban.center"] <- (WC.UC$WC.UC.dist.km <= (25+(2*(((WC.UC$Area)/pi)**0.5))))
WC.UC <- WC.UC[order(-WC.UC$pop, na.last=TRUE) , ]

# Assigning a 1 to all the largest cities in a country
LC <- aggregate(pop ~ country.etc, WC.UC, max)
LC["largestC"] <- 1 
WC.UC <- merge(WC.UC, LC, by=c("pop", "country.etc"), all.x=TRUE)
WC.UC <- WC.UC[order(-WC.UC$pop), ]
WC.UC$largestC[is.na(WC.UC$largestC)] <-0
WC.UC$largestC[WC.UC$merge == "indiabombay"] <- 1
WC.UC$largestC[WC.UC$merge == "mexicomexicocity"] <- 1
WC.UC$largestC[WC.UC$merge == "unitedstatesofamericanewyork"] <- 1
WC.UC$largestC[WC.UC$merge == "unitedkingdomlondon"] <- 1
WC.UC$largestC[WC.UC$merge == "myanmaryangon"] <- 1
rm(LC)


#Housekeeping
WC.UC$lat.y <- NULL
WC.UC$lat <- WC.UC$lat.x
WC.UC.dist <- subset(WC.UC, select=c(pop, country.etc, merge, name, lat, long, capital, Region, Closest.Urban.Center, 
                                Population, WC.UC.dist.km, Area, Density, coastalMC, largest.UC, part.of.urban.center,
                                largestC))

write.csv(WC.UC, "Cache/WC.UC.dist.old.csv")



#############################################################################################################
################################################ 4. Manipulating  ###########################################
#############################################################################################################
WC.UC.dist <- read.csv("Cache/WC.UC.dist.old.csv")


# Country level data from the World Bank Development Indicators (WDI) and the The Correlates of War (COW) project data on wars.
if(file.exists("Cache/CountryData.csv")){CountryData <- read.csv("Cache/CountryData.csv")} else{source("1.b - Country Data.R")}

# Now with both City and Country Data gathered, we can fill some gaps in the Wold Banks largest city population indicator
source('SmallScripts/fill.EN.URB.LCTY.UR.R')

# Bring Country level Data for 2010 into the WC.UC Data for the purpose of changing city population to fit WDI aggregates
G <- WC.UC.dist
G["realpop"] <- G$pop
G["year"] <- 2010
G["country"] <- G$country.etc
GM <- merge(G, CountryData, by=c("country", "year"), all.x=TRUE)
G <- subset(GM, select=c(realpop, country, name, capital, pop, largestC, part.of.urban.center, 
                         Closest.Urban.Center, largest.UC, Population,
                         EN.URB.LCTY.UR, EN.URB.MCTY, SP.URB.TOTL,SP.POP.TOTL))
rm(GM)

# if largest City was attacked, we use the EN.URB.LCTY.UR 
#(WDI for largest city population) instead of the old population
G$realpop <- ifelse(G$largestC==1&(!is.na(G$EN.URB.LCTY.UR)),
                    G$EN.URB.LCTY.UR, G$realpop)

# if UC was attacked, we use the UC Population instead of the previous city population
G$realpop <- ifelse(G$part.of.urban.center==TRUE, G$Population, G$realpop)

# if UC attacked & its the largest UC, use EN.URB.LCTY.UR
G$realpop <- ifelse((G$part.of.urban.center==TRUE&(!is.na(G$EN.URB.LCTY.UR))), 
                    (ifelse(G$largest.UC==1,G$EN.URB.LCTY.UR, G$realpop)), G$realpop)


#################################
# finding sums in the WC.UC Data  for the 3 categories (largest, UC and rest) we have WDI Data for

# At this point, it makes sense to bring UC name in the form of city-like name
X <- G$Closest.Urban.Center
X <- gsub(".*,","",X)
source('SmallScripts/CleanSpecialCharacters.R')
G["UC"] <- X
rm(X)

# sum of the population living in urban conglomerates of over 1m, excluding the largest city
G.UC.SUMS <- subset(G, select = c(realpop, country, name, pop, Population, EN.URB.LCTY.UR, EN.URB.MCTY, SP.URB.TOTL,
                                  SP.POP.TOTL, part.of.urban.center, UC), realpop >= 999999)
G.UC.SUMS$name <- ifelse((G.UC.SUMS$part.of.urban.center==TRUE), G.UC.SUMS$UC, G.UC.SUMS$name)
G.UC.SUMS$pop <-NULL
G.UC.SUMS$row.names <-NULL
G.UC.SUMS<-G.UC.SUMS[!duplicated(G.UC.SUMS$realpop),]
G.UC.SUMS2<-aggregate(G.UC.SUMS$realpop, by=list(country=G.UC.SUMS$country), FUN=sum)
G.UC.SUMS2["SUM.fake.MCTY"] <- G.UC.SUMS2$x
G.UC.SUMS2$x <- NULL
G <- merge(G, G.UC.SUMS2, by=c("country"), all.x=TRUE)
rm(G.UC.SUMS2, G.UC.SUMS)
G.SC.SUMS <- subset(G, select = c(realpop, country, name, pop, Population, EN.URB.LCTY.UR, EN.URB.MCTY, SP.URB.TOTL,
                                  SP.POP.TOTL, part.of.urban.center, UC), realpop <= 999999)
G.SC.SUMS$name <- ifelse((G.SC.SUMS$part.of.urban.center==TRUE), G.SC.SUMS$UC, G.SC.SUMS$name)
G.SC.SUMS$pop <-NULL
G.SC.SUMS$row.names <-NULL
G.SC.SUMS<-G.SC.SUMS[!duplicated(G.SC.SUMS$realpop),]
G.SC.SUMS2<-aggregate(G.SC.SUMS$realpop, by=list(country=G.SC.SUMS$country), FUN=sum)
G.SC.SUMS2["SUM.fake.REST"] <- G.SC.SUMS2$x
G.SC.SUMS2$x <- NULL
G <- merge(G, G.SC.SUMS2, by=c("country"), all.x=TRUE)

G["Restpop.WDI"]<- G$SP.URB.TOTL - G$EN.URB.MCTY
G["MCTYpop.WDI"]<- G$EN.URB.MCTY - G$EN.URB.LCTY.UR


# bring all UC-bound cities back to their original estimate 
G$realpop <- ifelse(G$part.of.urban.center==TRUE, G$pop, G$realpop)
G$realpop <- ifelse(G$largestC==1&(!is.na(G$EN.URB.LCTY.UR)),G$EN.URB.LCTY.UR, G$realpop)

# chaning the size of all cities under 1m inhabitants so the county sum fits the WDI
G$realpop <- ifelse((G$realpop!=G$EN.URB.LCTY.UR)& (G$realpop<=999999)&(!is.na(G$EN.URB.LCTY.UR))&(!is.na(G$SUM.fake.REST))&
                      (!is.na(G$Restpop.WDI))&(G$Restpop.WDI>=999999)&(G$SUM.fake.REST>=999999), (G$realpop*G$Restpop.WDI/G$SUM.fake.REST), G$realpop)

# chaning the size of all cities over 1m inhabitants so the county sum fits the WDI
G$realpop <- ifelse((G$realpop!=G$EN.URB.LCTY.UR)&(G$realpop>=999999)&(!is.na(G$EN.URB.LCTY.UR))&(!is.na(G$SUM.fake.MCTY))&
                      (!is.na(G$MCTYpop.WDI))&(G$MCTYpop.WDI>=1999999)&(G$SUM.fake.REST>=1999999), (G$realpop*G$MCTYpop.WDI/G$SUM.fake.MCTY), G$realpop)


# bringing it back into the WC.UC.dist
G <-G[order(-G$pop, G$name), ]
WC.UC.dist <-WC.UC.dist[order(-WC.UC.dist$pop, WC.UC.dist$name), ]
WC.UC.dist["old.pop"] <- WC.UC.dist$pop
WC.UC.dist$pop <- G$realpop

WC.UC.dist["old.name"] <- as.character(WC.UC.dist$name)
WC.UC.dist$name <- ifelse((G$part.of.urban.center==TRUE), as.character(G$UC), as.character(WC.UC.dist$old.name))

rm(G.SC.SUMS2, G.SC.SUMS)
rm(G)

#write csv
write.csv(WC.UC.dist, "Cache/WC.UC.dist.csv")




#################### Load some GIS Data into the set ########################

C <- read.csv("Cache/WC.UC.dist.csv")

unzip("Downloaded_Data/Downloaded_Raster_Data.zip", exdir="Downloaded_Data")

###
# Distance to coast in km 
# http://worldgrids.org/doku.php?id=wiki:layers

RASTERcoastdist <- raster("Downloaded_Data/DICGSH1a.tif")
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["coast.dist"] <- raster::extract(RASTERcoastdist, p1)
C["coast.dist"] <- ifelse(p1$coast.dist>=0, 0, round((p1$coast.dist^2)^0.5))
rm(RASTERcoastdist)


###
# Urban Land Cover in %
# http://worldgrids.org/doku.php?id=wiki:g19esa3

RASTERurban <- raster("Downloaded_Data/G19ESA3a.tif")
RASTERurban <- aggregate(RASTERurban, fact=20, fun=max, na.rm=TRUE) # 1km to 20km raster max
RASTERurban <- aggregate(RASTERurban, fact=2, fun=mean, na.rm=TRUE) # 20km to 40km raster mean
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["urbn.cover"] <- raster::extract(RASTERurban, p1, method='bilinear')
C["urbn.cover"] <- round(p1$urbn.cover)
rm(RASTERurban)


###
# travel time to major city in minutes (2000) 
# http://worldgrids.org/doku.php?id=wiki:layers

RASTERacess <- raster("Downloaded_Data/GACGEM2a.tif")
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["access"] <- raster::extract(RASTERacess, p1)
C["access"] <- round(p1$access)
unlink("Downloaded_Data/GACGEM2a.tif")
rm(RASTERacess)

###
# Stable light shining at night in mean reflection levels 1-63 from 1992 to 2010 
# http://worldgrids.org/doku.php?id=wiki:layers

RASTERlight<- raster("Downloaded_Data/LNMDMS2a.tif")
RASTERlight <- aggregate(RASTERlight, fact=2, fun=max, na.rm=TRUE)
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["light"] <- raster::extract(RASTERlight, p1, method='bilinear')
C["light"] <- round(p1$light)
rm(RASTERlight)

###
# Economic activity in millions of dollars per km2 (2006)
# http://ngdc.noaa.gov/eog/dmsp/download_gdp.html

download.file("http://ngdc.noaa.gov/eog/data/GDP/GDP_grid_flt.tif.gz", "Downloaded_Data/GDP_grid_flt.tif.gz")
gunzip("Downloaded_Data/GDP_grid_flt.tif.gz", destname="Downloaded_Data/GDP_grid_flt.tif")

RASTERgdp <- raster("Downloaded_Data/GDP_grid_flt.tif")
RASTERgdp <- aggregate(RASTERgdp, fact=10, fun=max, na.rm=TRUE) # 1 to 10km Radius max
RASTERgdp <- aggregate(RASTERgdp, fact=2, fun=mean, na.rm=TRUE) # 10km to 20km raster mean
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["gdp"] <- raster::extract(RASTERgdp, p1, method='bilinear')
C["city.gdp"] <- round(p1$gdp)
rm(RASTERgdp)

###
# The Night Light Development Index for 2006, exquality in distribution of light (0-1 Lorenz Curve)
# ngdc.noaa.gov 

RASTERnldi <- raster("Downloaded_Data/NLDI_2006_0p25_rev20111230.tif")
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["nldi"] <- raster::extract(RASTERnldi, p1, method='bilinear')
C["nldi"] <- ifelse(p1$nldi<=0 | p1$nldi>=1, NA, as.numeric(p1$nldi))
rm(RASTERnldi)


###
# Population Density with 50km resolution (1990)
# NASA's Earth Observing System Data and Information System

RASTER90pop <- raster("Downloaded_Data/glds90ag30.asc")
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["dens.90"] <- raster::extract(RASTER90pop, p1, method='bilinear')
C["dens.90"] <- ifelse(is.na(p1$dens.90), 0, round(p1$dens.90))

# Plus Countries Maximum
Rank.POP90.MAX<-aggregate(C$dens.90, by=list(C$country.etc), FUN=max)
colnames(Rank.POP90.MAX)[1] <- "country.etc"
colnames(Rank.POP90.MAX)[2] <- "dens.90.MAX"
C <- merge(C, Rank.POP90.MAX, by=c("country.etc"), all.x=TRUE)
rm(Rank.POP90.MAX, RASTER90pop)


###
# Population Density with 50km resolution (1995)
# NASA's Earth Observing System Data and Information System

RASTER95pop <- raster("Downloaded_Data/glds95ag30.asc")
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["dens.95"] <- raster::extract(RASTER95pop, p1, method='bilinear')
C["dens.95"]<- ifelse(is.na(p1$dens.95), 0, round(p1$dens.95))

# Plus Countries Maximum
Rank.POP95.MAX<-aggregate(C$dens.95, by=list(C$country.etc), FUN=max)
colnames(Rank.POP95.MAX)[1] <- "country.etc"
colnames(Rank.POP95.MAX)[2] <- "dens.95.MAX"
C <- merge(C, Rank.POP95.MAX, by=c("country.etc"), all.x=TRUE)
rm(Rank.POP95.MAX, RASTER95pop)


###
# Population Density with 50km resolution (2000)
# NASA's Earth Observing System Data and Information System

RASTER00pop <- raster("Downloaded_Data/glds00ag30.asc")
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["dens.00"] <- raster::extract(RASTER00pop, p1, method='bilinear')
C["dens.00"] <- ifelse(is.na(p1$dens.00), 0, round(p1$dens.00))

# Plus Countries Maximum
Rank.POP00.MAX<-aggregate(C$dens.00, by=list(C$country.etc), FUN=max)
colnames(Rank.POP00.MAX)[1] <- "country.etc"
colnames(Rank.POP00.MAX)[2] <- "dens.00.MAX"
C <- merge(C, Rank.POP00.MAX, by=c("country.etc"), all.x=TRUE)
rm(Rank.POP00.MAX, RASTER00pop)


###
# Population Density Estimate with 50km resolution (2005)
# NASA's Earth Observing System Data and Information System

RASTER05pop <- raster("Downloaded_Data/glds05ag30.asc")
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["dens.05"] <- raster::extract(RASTER05pop, p1, method='bilinear')
C["dens.05"] <- ifelse(is.na(p1$dens.05), 0, round(p1$dens.05))

# Plus Countries Maximum
Rank.POP05.MAX<-aggregate(C$dens.05, by=list(C$country.etc), FUN=max)
colnames(Rank.POP05.MAX)[1] <- "country.etc"
colnames(Rank.POP05.MAX)[2] <- "dens.05.MAX"
C <- merge(C, Rank.POP05.MAX, by=c("country.etc"), all.x=TRUE)
rm(Rank.POP05.MAX, RASTER05pop)


###
# Population Density Estimate with 50km resolution (2010)
# NASA's Earth Observing System Data and Information System

RASTER10pop <- raster("Downloaded_Data/glds10ag30.asc")
p1 <- data.frame(lon=C$lon, lat=C$lat)
p1["dens.10"] <- raster::extract(RASTER10pop, p1, method='bilinear')
C["dens.10"] <- ifelse(is.na(p1$dens.10), 0, round(p1$dens.10))

# Plus Countries Maximum
Rank.POP10.MAX<-aggregate(C$dens.10, by=list(C$country.etc), FUN=max)
colnames(Rank.POP10.MAX)[1] <- "country.etc"
colnames(Rank.POP10.MAX)[2] <- "dens.10.MAX"
C <- merge(C, Rank.POP10.MAX, by=c("country.etc"), all.x=TRUE)
rm(Rank.POP10.MAX, RASTER10pop)


WC.UC.dist.gis <- C
rm(C, p1)


write.csv(WC.UC.dist.gis, "Cache/WC.UC.dist.gis.csv")

unlink("Downloaded_Data/NLDI_2006_0p25_rev20111230.tif")
unlink("Downloaded_Data/GDP_grid_flt.tif")
unlink("Downloaded_Data/GDP_grid_flt.tif.gz")
unlink("Downloaded_Data/LNMDMS2a.tif")
unlink("Downloaded_Data/DICGSH1a.tif")
unlink("Downloaded_Data/G19ESA3a.tif")
