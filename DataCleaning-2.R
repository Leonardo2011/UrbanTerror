


# subset and renaming so the two datasets match in their columns
worldcities2013 <- subset(worldcities2013, select =c("City", "Country", "Population", "Latitude", "Longitude", "Region"))
colnames(worldcities2013)[1] <- "name"
colnames(worldcities2013)[2] <- "country.etc"
colnames(worldcities2013)[3] <- "pop"
colnames(worldcities2013)[4] <- "lat"
colnames(worldcities2013)[5] <- "long"

#create a column to merge over: countrycity
world.cities2009$merge <- paste(world.cities2009$country.etc, world.cities2009$name, sep="")
worldcities2013$merge <- paste(worldcities2013$country.etc, worldcities2013$name, sep="")

#merge to new set: "world.cities"
world.cities <- merge(world.cities2009, worldcities2013, by= c("merge", "name", "country.etc", "pop", "lat", "long"), all=TRUE)
world.cities <- world.cities[order(world.cities$merge, world.cities$capital, world.cities$pop),]
world.cities <- world.cities[!duplicated(world.cities$merge), ]
world.cities$merge <-NULL
world.cities <- world.cities[order(-world.cities$pop), ]
rm(worldcities2013, world.cities2009)

# bring country names in combined world.cities to WDi standart
source('SmallScripts/bring_country_names_in_citydata_to_WDI.R')

# create uniform country names in world.cities without special characters and lowcase
X <- world.cities$country.etc
source('SmallScripts/delete_country_special_characters.R')
world.cities$country.etc <- X
rm(X)


#########################################
########## URBAN CENTERS DATA ###########
#########################################

# Scrap Wiki on urban Centers
URL <- 'http://en.wikipedia.org/w/index.php?title=List_of_urban_areas_by_population&section=2'
table <- readHTMLTable(URL, encoding = "UTF-16")
UrbanCenters <- table [[2]]
UrbanCenters$City <- as.character(UrbanCenters$City)
colnames(UrbanCenters)[5] <- "Area"
colnames(UrbanCenters)[6] <- "Density"

#clean up the Urban Centers name in order to allow google.maps API to find them
source('SmallScripts/UC_Cleaning.R')

# put together a string with "Country, City" to allow google.maps API to find them
b <-data.frame(paste(UrbanCenters$Country, UrbanCenters$City, sep=", "), row.names = NULL)
b["loc"] <- b
b$loc <- as.character(b$loc)
b$loc <- gsub("^..", "", b$loc)
a<-(b$loc)

# look up lon/lat data via google maps :: the geocode function of the package maps
#!!  as this process is time consuming, we saved the result as csv for the moment !!
#UrbanLoc <- geocode(a, output = c("latlon", "latlona", "more", "all"),messaging = FALSE, sensor = FALSE, override_limit = FALSE)
UrbanLoc <- read.csv("City Data/UrbanLoc.csv")

# bring the geo data back in the original data frame of urban centers
UrbanCenters["lat"] <- UrbanLoc$lat
UrbanCenters["lon"] <- UrbanLoc$lon
UrbanCenters["full name"] <- a
UrbanCenters$City <- tolower(UrbanCenters$City)

# delete whats not needed anymore
rm(UrbanLoc, table ,URL, b, a)

# put in costal megacities
source('SmallScripts/coastalcities.R')



########################################################################################
########################################################################################
##########################   MERGING  CITY DATA    #####################################
########################################################################################
########################################################################################

#!!!!!!!!!!!!!!!!!!!!!!#
#!!!!TIME CONSUMING!!!!#
#!!!!!!!!!!!!!!!!!!!!!!#

############################################
# Merging Urban Centers with world.cities with respective distance of each City to closes Urban Censter

# renaming colums and select sub-sets for merging over fake variable to create Matrix City X Urban (~ 60.000 Cities X ~ 500 urban Centers) 

UCmerge <- subset(UrbanCenters, select = c("lon", "lat", "full name","Population", "Area"))
UCmerge$fake=1
WCmerge <-subset(world.cities, select = c("long", "lat"))
WCmerge["CityID"] <- rownames(world.cities)
WCmerge$fake=1
Zillion <-merge(UCmerge, WCmerge, by=c("fake"))

# find each distance ( ~ 30 million individual distances will be found )
Zillion["DISTkm"] <- gdist(Zillion$lon, Zillion$lat.x, Zillion$long, Zillion$lat.y, units = "km", a = 6378137.0, b = 6356752.3142, verbose = FALSE)

# reduce to only the closest urban center for each and every city, 30 million distances to the ~ 60.000 minimal ones
Zillion.min <- aggregate(DISTkm ~ CityID, Zillion, function(x) min(x))

Zillion.fullmin <- merge(Zillion.min, Zillion, by=c("CityID", "DISTkm"))
Zillion.fullmin["Closest.Urban.Center"] <- Zillion.fullmin$"full name"
Zillion.fullmin["CUC.dist.km"] <- Zillion.fullmin$"DISTkm"

# bring information on closest urban center and the respective distance back into  'world.cities'
UC.WC.merger <- subset(Zillion.fullmin, select = c("CityID", "Closest.Urban.Center", "CUC.dist.km", "Area"))

# new dataset WC.UCdist! which stands for a merged dataset including distance and estimate if the respective city is part of an urban center
world.cities["CityID"] <-rownames(world.cities)
WC.UC.dist <- merge(world.cities, UC.WC.merger, by="CityID")
WC.UC.dist$CUC.dist.km <- as.numeric(WC.UC.dist$CUC.dist.km) 
WC.UC.dist$Area <- as.numeric(WC.UC.dist$Area) 
WC.UC.dist$capital <- as.numeric(WC.UC.dist$capital)


WC.UC.dist["part.of.urban.center"] <- (WC.UC.dist$CUC.dist.km <= (20+(2*(((WC.UC.dist$Area)/pi)**0.5))))
WC.UC.dist["in.urban.centers.environment"] <- (WC.UC.dist$CUC.dist.km<=(40+(3*(((WC.UC.dist$Area)/pi)**0.5))))
WC.UC.dist <- WC.UC.dist[order(-WC.UC.dist$pop, na.last=TRUE) , ]

# minor repairs
WC.UC.dist$capital[WC.UC.dist$name == "newdelhi" &  WC.UC.dist$country  == "india"] <- 1
WC.UC.dist$capital[WC.UC.dist$name == "peking" &  WC.UC.dist$country  == "china"] <- 1
WC.UC.dist$capital[WC.UC.dist$name == "beirut" &  WC.UC.dist$country  == "lebanon"] <- 1
WC.UC.dist$capital[WC.UC.dist$name == "guatemalacity" &  WC.UC.dist$country  == "guatemala"] <- 1

#remove rest
rm(WCmerge, UCmerge, Zillion, Zillion.min, Zillion.fullmin, UC.WC.merger)

# try to create a PreGTD
source('PreAnalysis/createpregtd.R') 
