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
  colnames(UrbanCenters)[5] <- "Area"
  colnames(UrbanCenters)[6] <- "Density"
  
  
  ##### Cleaning Data #####
  
  #Cleaning the Urban Centers name in order to allow google.maps API to find them
  source('SmallScripts/UrbanCleaning.R')
  
  ###### Manipulate Data ######
  
  #Putting in a string with "Country, City" to allow google.maps API to find them
  CoCi <-data.frame(paste(UrbanCenters$Country, UrbanCenters$City, sep=", "), row.names = NULL)
  CoCi["loc"] <- CoCi
  CoCi$loc <- as.character(CoCi$loc)
  CoCi$loc <- gsub("^..", "", CoCi$loc)
  CoCiLoc<-(CoCi$loc)
  
  #Looking up lon/lat data for each Urban Center via google.maps using the geocode function of the package maps.
  #This is a very time consuming process, which is why we chose to cache the result to reduce computation time for this script.
  if(file.exists("Cache/UrbanLoc.csv"))
  {UrbanLoc <- read.csv("Cache/UrbanLoc.csv")
  } else
  {
    UrbanLoc <- geocode(CoCiLoc, output = c("latlon", "latlona", "more", "all"),messaging = FALSE, sensor = FALSE, override_limit = FALSE)
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
  UrbanCenters$largest.UC <- recode(UrbanCenters$largest.UC, "NA=0")

  
  #Including dummy variables for coastal megacities
  source('SmallScripts/CoastalMC.R')
  
  #Deleting what is not needed anymore
  rm(UrbanLoc, table ,URL, CoCi, CoCiLoc, LUC)
  
  #Caching the Urban Centers dataframe
  write.csv(UrbanCenters, "Cache/UrbanCenters.csv")
  
}


#############################################################################################################
###################################### 2. Both City Data Sets ###############################################
#############################################################################################################


if(file.exists("Cache/world.cities.csv"))
{world.cities <- read.csv("Cache/world.cities.csv")
} else
{
  
  #############  City Data  ##############
  
  #The purpose of this script is to gather, clean, and merge two different datasets on cities of the world with similar information.
  #We merge them to increase the total number of cities that could match the GTD.
  
  
  ###### Gathering Data  ######
  
  ###World cities dataset 1/2 (worldcities2013)###
  
  # it stems from a private company, Maxmind Inc.
  #http://download.maxmind.com/download/worldcities/worldcitiespop.txt.gz and transformed into CSV, since it is not available
  #in .csv format right away.
  worldcities2013 <- read.csv("Downloaded_Data/worldcitiespop.csv")
  
  ###World cities dataset 2/2 (world.cities from the 'maps' package)###
  
  #The dataset is called world.cities. Since it is from 2009, we call it worldcities2009.
  data(world.cities)
  worldcities2009 <- world.cities
  rm(world.cities)
  
  
  ###### Cleaning Data  ######
  
  #Our standard for city names is lowercase letters with no special characters from the worldcities2009 dataset 
  #for easier merging purposes, and lowercase country names (no special charackters) from the world.cities2013 dataset.
  
  ###World cities dataset 1/2 (worldcities2013)###
  
  #The dataset lacks some information.
  
  #introduce Tehran as it was missing in the original dataset
  worldcities2013 <- rbind(worldcities2013, data.frame(X=0,Country="ir", City="tehran", AccentCity="Tehran", 
                                                       Region= 1, Latitude=35.67, Longitude=51.43,Population=7160094))
  
  #introduce Akkaraipattu as it was missing in the original dataset
  worldcities2013 <- rbind(worldcities2013, data.frame(X=0,Country="lk", City="Akkaraipattu", AccentCity="Akkaraipattu", 
                                                       Region= 31, Latitude=7.227862, Longitude=81.850551,Population=35000))
  
  # Change Country Code to Countries on the level of the other set
  source('SmallScripts/CountryCode.R')
  worldcities2013$Country <- tolower(worldcities2013$Country)
  
  #Recoding city names to our standard
  worldcities2013$City <- gsub(" ", "", worldcities2013$City)
  worldcities2013$City <- tolower(worldcities2013$City)
  
  
  
  ###World cities dataset 2/2 (worldcities2009 from the 'maps' package)###
  
  worldcities2009$name <- tolower(worldcities2009$name)
  
  #the dataset has missing or wrong information
  worldcities2009$capital[worldcities2009$name == "delhi" & worldcities2009$country.etc == "India"] <- "1"
  worldcities2009$name[worldcities2009$name == "soul" & worldcities2009$country.etc == "Korea South"] <- "seoul"
  worldcities2009$name[worldcities2009$name == "bombay" &  worldcities2009$country.etc  == "India"] <- "mumbai"
  worldcities2009$name[worldcities2009$name == "new york" &  worldcities2009$country.etc  == "USA "] <- "newyorkcity"
  
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
  worldcities2009$country.etc<-gsub(" ", "",worldcities2009$country.etc, ignore.case=TRUE)
  worldcities2009$country.etc<-gsub("\\,", "",worldcities2009$country.etc, ignore.case=TRUE)
  worldcities2009$country.etc <- tolower(worldcities2009$country.etc)
  worldcities2009$name <- gsub(" ", "", worldcities2009$name)
  worldcities2009$name <- tolower(worldcities2009$name)
  
  
  ###### Manipulate Data  ######
  
  #We now merge the two datasets into one. First, some preparation.
  
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
  
  # Assigning a 1 to all the largest cities in a country
  LC <- aggregate(pop ~ country.etc,world.cities,max)
  LC["largestC"] <- 1
  world.cities <- merge(world.cities, LC, by=c("pop", "country.etc"), all.x=TRUE)
  world.cities <- world.cities[order(-world.cities$pop), ]
  world.cities$largestC <- recode(world.cities$largestC, "NA=0")
  rm(LC)
  
  
  #Bringing country names in combined world.cities to WDI lowercase standard
  source('SmallScripts/CountryCleaninginCitySet.R')
  X <- world.cities$country.etc
  source('SmallScripts/CleanSpecialCharacters.R')
  world.cities$country.etc <- X
  rm(X)
  
  #create cache world.cities.csv
  write.csv(world.cities, "Cache/world.cities.csv")
    
}


#############################################################################################################
####################### 3. Merging Data Both City Data Sets and Urban Centers ###############################
#############################################################################################################


#In this script we merge the UrbanCenters and world.cities databases so that each 
#city is allocated to its closest Urban Center (including distances)


#Renaming columns and select subsets for merging over fake variable to create a matrix 
#with each City X each Urban Center (~ 60.000 Cities X ~ 500 urban Centers) 
UCmerge <- subset(UrbanCenters, select = c("lon", "lat", "full.name"))
UCmerge$fake=1
WCmerge <-subset(world.cities, select = c("long", "lat"))
WCmerge["CityID"] <- rownames(world.cities)
WCmerge$fake=1

ALLDIST <-merge(UCmerge, WCmerge, by=c("fake"))

#Finding each distance ( ~ 30 million individual distances will be found )
ALLDIST["DISTkm"] <- gdist(ALLDIST$lon, ALLDIST$lat.x, ALLDIST$long, ALLDIST$lat.y, units = "km",
                           a = 6378137, b = 6356752, verbose = FALSE)

#Reducing to only the closest Urban Center for each and every city, 30 million distances to the ~ 60.000 minimal ones
ALLDIST.min <- aggregate(DISTkm ~ CityID, ALLDIST, function(x) min(x))
ALLDIST$lon <- NULL
ALLDIST$lat.x	<- NULL		
ALLDIST$long <- NULL
ALLDIST$lat.y <- NULL
ALLDIST.fullmin <- merge(ALLDIST.min, ALLDIST, by=c("CityID", "DISTkm"))
ALLDIST.fullmin <- merge(ALLDIST.fullmin, UrbanCenters, by=c("full.name"), all.x=TRUE)
ALLDIST.fullmin["Closest.Urban.Center"] <- ALLDIST.fullmin$"full.name"
ALLDIST.fullmin["WC.UC.dist.km"] <- ALLDIST.fullmin$"DISTkm"

#Bringing information on closest Urban Center and the respective distance back into 'world.cities'
UC.WC.merger <- subset(ALLDIST.fullmin, select = c("CityID", "Closest.Urban.Center", "Population", "WC.UC.dist.km", "Area", "Density", "coastalMC", "largest.UC"))

#New dataset WC.UC.dist! which stands for a merged dataset including distance and estimate if the respective city is part of an urban center
world.cities["CityID"] <-rownames(world.cities)
WC.UC.dist <- merge(world.cities, UC.WC.merger, by="CityID")
WC.UC.dist$WC.UC.dist.km <- as.numeric(WC.UC.dist$WC.UC.dist.km) 
WC.UC.dist$Area <- as.numeric(WC.UC.dist$Area) 
WC.UC.dist$capital <- as.numeric(WC.UC.dist$capital)

#CityID is now not needed anymore.
WC.UC.dist$CityID <- NULL

#Housekeeping
X<-WC.UC.dist$name
source('SmallScripts/CleanSpecialCharacters.R')
WC.UC.dist$name <- X
rm(X)

X<-WC.UC.dist$country.etc
source('SmallScripts/CleanSpecialCharacters.R')
source('SmallScripts/CountryCleaninginCitySet.R')
WC.UC.dist$country.etc <- X
rm(X)

#Introducing logical variable that tells us whether a city is part of an Urban Center or close to one.
#We use a calculation that treates the middle of the city as the center of a circle.
WC.UC.dist["part.of.urban.center"] <- (WC.UC.dist$WC.UC.dist.km <= (20+(2*(((WC.UC.dist$Area)/pi)**0.5))))
WC.UC.dist["in.urban.centers.environment"] <- (WC.UC.dist$WC.UC.dist.km<=(40+(3*(((WC.UC.dist$Area)/pi)**0.5))))
WC.UC.dist <- WC.UC.dist[order(-WC.UC.dist$pop, na.last=TRUE) , ]


#remove rest
rm(WCmerge, UCmerge, ALLDIST, ALLDIST.min, ALLDIST.fullmin, UC.WC.merger)

write.csv(WC.UC.dist, "Cache/WC.UC.dist.old.csv")

#############################################################################################################
################################################ 4. Manitulating  ###########################################
#############################################################################################################


# Country level data from the World Bank Development Indicators (WDI) and the The Correlates of War (COW) project data on wars.
if(file.exists("Cache/CountryData.csv")){CountryData <- read.csv("Cache/CountryData.csv")} else{source("1.b - Country Data.R")}


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


# At this point, it makes sense to bring UC name in the form of city-like name
X <- G$Closest.Urban.Center
X <- gsub(".*,","",X)
source('SmallScripts/CleanSpecialCharacters.R')
G["UC"] <- X
rm(X)

# if UC was attacked, we use the UC Population instead of the previous city population
G$realpop <- ifelse(G$part.of.urban.center==TRUE, G$Population, G$realpop)

# if largest City was attacked, we use the EN.URB.LCTY.UR 
#(WDI for largest city population) instead of the old population
G$realpop <- ifelse((G$part.of.urban.center==FALSE&(G$largestC==1)&(!is.na(G$EN.URB.LCTY))), 
                    G$EN.URB.LCTY.UR, G$realpop)

# if UC attacked & its the largest UC > use EN.URB.LCTY.UR
G$realpop <- ifelse((G$part.of.urban.center==TRUE&(!is.na(G$EN.URB.LCTY))), 
                    (ifelse(G$largest.UC==1,G$EN.URB.LCTY, G$realpop)), G$realpop)


#################################
# finding sums in the WC.UC Data  for the 3 categories (largest, UC and rest) we have WDI Data for

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


# chaning the size of all cities under 1m inhabitants so the county sum fits the WDI
G$realpop <- ifelse((G$realpop!=G$EN.URB.LCTY)&(G$realpop<=999999)&(!is.na(G$EN.URB.LCTY))&(!is.na(G$SUM.fake.REST))&
                      (!is.na(G$Restpop.WDI))&(G$SUM.fake.REST!=0), (G$realpop*G$Restpop.WDI/G$SUM.fake.REST), G$realpop)

# chaning the size of all cities over 1m inhabitants so the county sum fits the WDI
G$realpop <- ifelse((G$realpop!=G$EN.URB.LCTY)&(G$realpop>=999999)&(!is.na(G$EN.URB.LCTY))&(!is.na(G$SUM.fake.MCTY))&
                      (!is.na(G$MCTYpop.WDI))&(G$SUM.fake.REST!=0), (G$realpop*G$MCTYpop.WDI/G$SUM.fake.MCTY), G$realpop)


# bringing it back into the WC.UC.dist
G <-G[order(-G$pop, G$name), ]
WC.UC.dist <-WC.UC.dist[order(-WC.UC.dist$pop, WC.UC.dist$name), ]
WC.UC.dist["old.pop"] <- WC.UC.dist$pop
WC.UC.dist$pop <- G$realpop

WC.UC.dist["old.name"] <- as.character(WC.UC.dist$name)
G["old.name"] <- as.character(G$name)
G$name <- ifelse((G$part.of.urban.center==TRUE), G$UC, G$old.name)
WC.UC.dist$name <- ifelse((G$part.of.urban.center==TRUE), G$UC, G$old.name)

rm(G.SC.SUMS2, G.SC.SUMS)
rm(G)

#write csv
write.csv(WC.UC.dist, "Cache/WC.UC.dist.csv")
