# MPP-E1180: Introduction to Collaborative Social Science Data Analysis
### Fall 2014
### Instructor: Christopher Gandrud

############################
####### URBAN TERROR #######
############################
###### Part 1: Data   ######
############################
#Lukas B Cameron R Sascha S#
############################
###### Cleaning Data  ######
############################


#Loading R packages using @stevenworthington's ipak.R gist from https://gist.github.com/stevenworthington/3178163.

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

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

########################################################################################
########################################################################################
############################   GATHERING  DATA    ######################################
########################################################################################
########################################################################################



############################################
###### The Global Terrorism Database  ######
############################################


############################################
#Load the Global Terrorism Database (GTD). It is open souce and can be downloaded after registration at 
# http://www.start.umd.edu/gtd/contact/

rawGTD <- read.csv("Terror Data/globalterrorismdb_0814dist.csv", header=TRUE)


#The (GTD) contains over a 120k observations on more than 120 variables. We don't need them all. 
#We therefore filter the database to make it fit our needs, erasing over a 100 variables. 
#We only want to look at successfull terror attacks and include basic data on time, location and target.

GTD <- subset(rawGTD, select = c(eventid, iyear, imonth, iday, country, country_txt, region, provstate, region_txt, city, attacktype1, targtype1, targsubtype1,
                                weaptype1, weapsubtype1, propextent, nkill, nwound), 
                                iyear >= 1970 & success == 1, na.strings = c("", " "))
rm(rawGTD)

#Next we order the GTD)
GTD <- GTD[order (GTD$country_txt, GTD$iyear, GTD$imonth, GTD$iday, GTD$city), ]

############################################
#We introduce our first scale: "Targets Urbanity Potential Scale (TUPscale)"

GTD["TUPscale"] <- GTD$targsubtype1
GTD$TUPscale <- recode(GTD$TUPscale, "40:42 = 9; 9 = 0; 27:35 = 0; 37:39 = 0; 65 = 0; 72 = 0; 1 = 2; 4:5 = 2; 10 = 2;
                       12 = 2; 53:56 = 2; 58:59 = 2; 61:62 = 2; 82 = 2; 95:96 = 2;6 = 9; 13 = 9; 104:108 = 9; 
                       51:52 = 9; 57 = 9; 60 = 9; 63:64 = 9; 73 = 9; 80:81 = 9; 88:92 = 9; 98 = 9; 2 = 7; 3 = 7; 
                       7:8 = 7; 44 = 7; 48:50 = 7; 67:71 = 7; 74:79 = 7; 83:87 = 7; 97 = 7; 99 = 7; 14:26 = 9; 
                       100:103 = 9; 111 = 9; 109 = 9; 110 = 9; 36 = 9; 43 = 9; 45:47 = 9; 66 = 9; 93:94 = 9; 
                       11 = 9", as.numeric.result=TRUE)


# 0= Rural & Military; 2= Government & Police; 3= Potentilly Urban Workplace; #7= Potentilly Urban Infrastructure; 
# 9= Potentilly Expression of Urban Core Life


############################################
# We introduce our second scale: "Extent of Property Damage (PROPscale)" and write it back into the GTD

GTD["PROPscale"] <- GTD$propextent
GTD$PROPscale <- as.numeric(GTD$PROPscale)

#Bring the values to the $ values coded in the originally coded categories. 
GTD$PROPscale <- recode(GTD$PROPscale, "1=1000000000; 2=1000000; 3=1000; 4=0; NA=NA")


############################################
# We introduce our third scale: "Extent of Human Damage (HUMscale)" which adds wounded and killed /and write it back into the GTD

GTD["HUMscale"] <- GTD$nkill+GTD$nwound
GTD$HUMscale <- as.numeric(GTD$HUMscale)



###########################################
############## BIG CITY DATA ##############
###########################################

# here: http://download.maxmind.com/download/worldcities/worldcitiespop.txt.gz and transformed into CSV
worldcities2013 <- read.csv("City Data/worldcitiespop.csv")

# introduce Tehran as it was missing in the original dataset
worldcities2013 <- rbind(worldcities2013, data.frame(X=0,Country="ir", City="tehran", AccentCity="Tehran", 
                                                     Region= 1, Latitude=35.67, Longitude=51.43,Population=7160094))

# introduce Akkaraipattu as it was missing in the original dataset
worldcities2013 <- rbind(worldcities2013, data.frame(X=0,Country="lk", City="Akkaraipattu", AccentCity="Akkaraipattu", 
                                                     Region= 31, Latitude=7.227862, Longitude=81.850551,Population=35000))

# sorting by population
worldcities2013 <- worldcities2013[order(-worldcities2013$Population, na.last=TRUE) , ]

source('SmallScripts/2digit2ctry.R')
worldcities2013$City <- gsub(" ", "", worldcities2013$City)
worldcities2013$City <- tolower(worldcities2013$City)





### list the world capital cities
data(world.cities)
world.cities$name <- tolower(world.cities$name)
world.cities2009 <- world.cities[order(-world.cities$pop, na.last=TRUE) , ]
rm(world.cities)

#The dataframe wrongly lists dehli as not being the capital of india, plus had a typo in seoul, which both we recode.
world.cities2009$capital[world.cities2009$name == "delhi" & world.cities2009$country.etc == "India"] <- "1"
world.cities2009$name[world.cities2009$name == "soul" & world.cities2009$country.etc == "Korea South"] <- "seoul"

world.cities2009$country.etc <- gsub("Russia","Russian Federation", world.cities2009$country.etc)
world.cities2009$country.etc <- gsub("UK","United Kingdom", world.cities2009$country.etc)
world.cities2009$country.etc <- gsub("USA","United States of America", world.cities2009$country.etc)
world.cities2009$country.etc <- gsub("Korea North","Korea, Democratic People's Republic of", world.cities2009$country.etc)
world.cities2009$country.etc <- gsub("Korea South","Korea, Republic of", world.cities2009$country.etc)
world.cities2009$country.etc <- gsub("Sicily","Italy", world.cities2009$country.etc)
world.cities2009$country.etc <- gsub("East Timor","Timor-Leste", world.cities2009$country.etc)
world.cities2009$country.etc <- gsub("Madeira","Portugal", world.cities2009$country.etc)
world.cities2009$country.etc <- gsub("Madiera","Portugal", world.cities2009$country.etc)


##############################################################################################
# merge the two sets: cities 2013 and cities 2009 to world.cities
world.cities2009$name <- gsub(" ", "", world.cities2009$name)
world.cities2009$name <- tolower(world.cities2009$name)
world.cities2009$country.etc<-gsub(" ", "",world.cities2009$country.etc, ignore.case=TRUE)
world.cities2009$country.etc <- tolower(world.cities2009$country.etc)
worldcities2013$Country <- tolower(worldcities2013$Country)
worldcities2013 <- subset(worldcities2013, select =c("City", "Country", "Population", "Latitude", "Longitude", "Region"))
colnames(worldcities2013)[1] <- "name"
colnames(worldcities2013)[2] <- "country.etc"
colnames(worldcities2013)[3] <- "pop"
colnames(worldcities2013)[4] <- "lat"
colnames(worldcities2013)[5] <- "long"
world.cities2009$country.etc<-gsub("\\,", "",world.cities2009$country.etc, ignore.case=TRUE)
world.cities2009$merge <- paste(world.cities2009$country.etc, world.cities2009$name, sep="")
worldcities2013$merge <- paste(worldcities2013$country.etc, worldcities2013$name, sep="")
world.cities <- merge(world.cities2009, worldcities2013, by= c("merge", "name", "country.etc", "pop", "lat", "long"), all=TRUE)
world.cities <- world.cities[order(world.cities$merge, world.cities$pop),]
world.cities<-world.cities[!duplicated(world.cities$merge), ]
world.cities$merge <-NULL
world.cities <- world.cities[order(-world.cities$pop), ]
rm(worldcities2013, world.cities2009)


#########################################
########## URBAN CENTERS DATA ###########
#########################################

# Scrap Wiki on urban Centers
URL <- 'http://en.wikipedia.org/w/index.php?title=List_of_urban_areas_by_population&section=2'
table <- readHTMLTable(URL, encoding = "UTF-16")
UrbanCenters <- table [[2]]
UrbanCenters$City <- as.character(UrbanCenters$City)

#clean up the Urban Centers name in order to allow google.maps API to find them
UrbanCenters$City <- gsub("\\xc3\xb3","o", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\(.*","", UrbanCenters$City)
UrbanCenters$City <- gsub("\\[.+?\\]","", UrbanCenters$City)
UrbanCenters$City <- gsub("\\(.+?\\)","", UrbanCenters$City)
UrbanCenters$City <- gsub("[[:digit:]]", "", UrbanCenters$City)
UrbanCenters$City <- gsub("\\xe2\x80\x93.*","", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("Region", "", UrbanCenters$City)
UrbanCenters$City <- gsub("Greater ", "", UrbanCenters$City)

# put together a string with "Country, City" to allow google.maps API to find them
b <-data.frame(paste(UrbanCenters$Country, UrbanCenters$City, sep=", "), row.names = NULL)
b["loc"] <- b
b$loc <- as.character(b$loc)
b$loc <- gsub("^..", "", b$loc)
a<-(b$loc)

# look up lon lat data via google maps / the geocode function of the package maps
UrbanLoc <- geocode(a, output = c("latlon", "latlona", "more", "all"),messaging = FALSE, sensor = FALSE, override_limit = FALSE)

# bring the geo data back in the original data frame of urban centers
UrbanCenters["lat"] <- UrbanLoc$lat
UrbanCenters["lon"] <- UrbanLoc$lon
UrbanCenters["full name"] <- a
UrbanCenters$City <- tolower(UrbanCenters$City)

# delete whats not needed anymore
rm(UrbanLoc)
rm(table)
rm(URL)
rm(b)
rm(a)


# put in costal megacities
UrbanCenters$costalMC=0
UrbanCenters$costalMC[UrbanCenters$City == "tokyo"] <- "1"
UrbanCenters$costalMC[UrbanCenters$City == "jakarta"] <- "1"
UrbanCenters$costalMC[UrbanCenters$City == "seoul"] <- "1"
UrbanCenters$costalMC[UrbanCenters$City == "shanghai"] <- "1"
UrbanCenters$costalMC[UrbanCenters$City == "manila"] <- "1"
UrbanCenters$costalMC[UrbanCenters$City == "karachi"] <- "1"
UrbanCenters$costalMC[UrbanCenters$City == "new york city"] <- "1"
UrbanCenters$costalMC[UrbanCenters$City == "sao paolo"] <- "1"




############################################
############# World Bank Data  #############
############################################


############################################
#Load the World Bank Data on the nominal Urban Population 
WB_Urban_Pop = WDI(indicator='SP.URB.TOTL', country='all', start=1970, end=2013)



########################################################################################
########################################################################################
############################   MERGING  DATA    ########################################
########################################################################################
########################################################################################






############################################
# Merging Urban Centers with world.cities with respective distance of each City to closes Urban Censter

# renaming colums and select sub-sets for merging over fake variable to find each distance (~2million)
colnames(UrbanCenters)[5] <- "Area"
colnames(UrbanCenters)[6] <- "Density"
UCmerge <- subset(UrbanCenters, select = c("lon", "lat", "full name", "Population", "Area", "Density", "costalMC"))
UCmerge$fake=1
WCmerge <-subset(world.cities, select = c("long", "lat"))
WCmerge["CityID"] <- rownames(world.cities)
WCmerge$fake=1
Zillion <-merge(UCmerge, WCmerge, by=c("fake"))

#function for distance
distance.UC <- function(data, logA, latA, logUC, latUC){
  gdist(data[, logA], data[, latA], data[, lonUC], data[, latUC], 
        units = "km", a = 6378137.0, b = 6356752.3142, verbose = FALSE)
}

# find all ~2million distances
Zillion["DISTkm"] <- gdist(Zillion$lon, Zillion$lat.x, Zillion$long, Zillion$lat.y, units = "km", a = 6378137.0, b = 6356752.3142, verbose = FALSE)

# reduce to only the closest urban center for each and every city in world.cities
Zillion.min <- aggregate(DISTkm ~ CityID, Zillion, function(x) min(x))
Zillion.fullmin <- merge(Zillion.min, Zillion, by=c("CityID", "DISTkm"))
Zillion.fullmin["CityID"] <- Zillion.fullmin$"CityID"
Zillion.fullmin["Closest.Uran.Center"] <- Zillion.fullmin$"full name"
Zillion.fullmin["CUC.dist.km"] <- Zillion.fullmin$"DISTkm"

# bring information on closest urban center and the respective distance back into world.cities 
UR.WC.merger <- subset(Zillion.fullmin, select = c("CityID", "Closest.Uran.Center", "CUC.dist.km", "Population", "Area", "Density", "costalMC"))

# new dataset WC09.UCdist!
world.cities["CityID"] <-rownames(world.cities)
WC09.UCdist <- merge(world.cities, UR.WC.merger, by="CityID")
WC09.UCdist$Area <- as.numeric(WC09.UCdist$Area)
WC09.UCdist["attack.on.urban.center"] <- (WC09.UCdist$CUC.dist.km<=(3*(((WC09.UCdist$Area)/pi)**0.5)))

#remove rest
rm(distance.UC, WCmerge, UCmerge, Zillion, Zillion.min, Zillion.fullmin)