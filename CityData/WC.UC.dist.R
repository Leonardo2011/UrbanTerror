############# URBAN TERROR #############
########################################
########### Part II: Merging ###########
########################################
#######Lukas B Cameron R Sascha S#######
########################################


#### Urban Centers & world.cities ###


#In this script we merge the UrbanCenters and world.cities databases so that each 
#city is allocated to its closest Urban Center (including distances)

###### Gathering Data  ######

if(file.exists("Cache/UrbanCenters.csv"))
{UrbanCenters <- read.csv("Cache/UrbanCenters.csv")
} else
{
  source("CityData/UrbanCenters.R")
}

if(file.exists("Cache/world.cities.csv"))
{world.cities <- read.csv("Cache/world.cities.csv")
} else
{
  source("CityData/world.cities.R")
}

###### Merging Data #####


#Renaming columns and select subsets for merging over fake variable to create a matrix 
#with each City X each Urban Center (~ 60.000 Cities X ~ 500 urban Centers) 
UCmerge <- subset(UrbanCenters, select = c("lon", "lat", "full.name","Population", "Area"))
UCmerge$fake=1

WCmerge <-subset(world.cities, select = c("long", "lat"))
WCmerge["CityID"] <- rownames(world.cities)
WCmerge$fake=1

Zillion <-merge(UCmerge, WCmerge, by=c("fake"))

#Finding each distance ( ~ 30 million individual distances will be found )
Zillion["DISTkm"] <- gdist(Zillion$lon, Zillion$lat.x, Zillion$long, Zillion$lat.y, units = "km",
                           a = 6378137.0, b = 6356752.3142, verbose = FALSE)

#Reducing to only the closest Urban Center for each and every city, 30 million distances to the ~ 60.000 minimal ones
Zillion.min <- aggregate(DISTkm ~ CityID, Zillion, function(x) min(x))

Zillion.fullmin <- merge(Zillion.min, Zillion, by=c("CityID", "DISTkm"))
Zillion.fullmin["Closest.Urban.Center"] <- Zillion.fullmin$"full.name"
Zillion.fullmin["CUC.dist.km"] <- Zillion.fullmin$"DISTkm"

#Bringing information on closest Urban Center and the respective distance back into 'world.cities'
UC.WC.merger <- subset(Zillion.fullmin, select = c("CityID", "Closest.Urban.Center", "CUC.dist.km", "Area"))

#New dataset WC.UCdist! which stands for a merged dataset including distance and estimate if the respective city is part of an urban center
world.cities["CityID"] <-rownames(world.cities)
WC.UC.dist <- merge(world.cities, UC.WC.merger, by="CityID")
WC.UC.dist$CUC.dist.km <- as.numeric(WC.UC.dist$CUC.dist.km) 
WC.UC.dist$Area <- as.numeric(WC.UC.dist$Area) 
WC.UC.dist$capital <- as.numeric(WC.UC.dist$capital)
#CityID is now not needed anymore.
WC.UC.dist$CityID <- NULL

#Housekeeping
X<-WC.UC.dist$name
source('SmallScripts/CleanSpecialCharacters.R')
rm(X)

X<-WC.UC.dist$country.etc
source('SmallScripts/CleanSpecialCharacters.R')
rm(X)

#Introducing logical variable that tells us whether a city is part of an Urban Center or close to one.
#We use a calculation that treates the middle of the city as the center of a circle.
WC.UC.dist["part.of.urban.center"] <- (WC.UC.dist$CUC.dist.km <= (20+(2*(((WC.UC.dist$Area)/pi)**0.5))))
WC.UC.dist["in.urban.centers.environment"] <- (WC.UC.dist$CUC.dist.km<=(40+(3*(((WC.UC.dist$Area)/pi)**0.5))))
WC.UC.dist <- WC.UC.dist[order(-WC.UC.dist$pop, na.last=TRUE) , ]


#remove rest
rm(WCmerge, UCmerge, Zillion, Zillion.min, Zillion.fullmin, UC.WC.merger)

write.csv(WC.UC.dist, "Cache/WC.UC.dist.csv")
