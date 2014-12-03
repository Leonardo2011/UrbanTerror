world.cities1 <- subset (world.cities, pop<=median(pop))
world.cities2 <- subset (world.cities, !pop<=median(pop))

#In this script we merge the UrbanCenters and world.cities databases so that each 
#city is allocated to its closest Urban Center (including distances)


#Renaming columns and select subsets for merging over fake variable to create a matrix 
#with each City X each Urban Center (~ 60.000 Cities X ~ 500 urban Centers) 
UCmerge <- subset(UrbanCenters, select = c("lon", "lat", "full.name"))
UCmerge$fake=1
WCmerge <-subset(world.cities1, select = c("long", "lat"))
WCmerge["CityID"] <- rownames(world.cities1)
WCmerge$fake=1

ALLDIST <-merge(UCmerge, WCmerge, by=c("fake"))

#Finding each distance ( ~ 30 million individual distances will be found )
ALLDIST["DISTkm"] <- gdist(ALLDIST$lon, ALLDIST$lat.x, ALLDIST$long, ALLDIST$lat.y, units = "km",
                           a = 6378137, b = 6356752, verbose = FALSE)

#Reducing to only the closest Urban Center for each and every city, 30 million distances to the ~ 60.000 minimal ones
ALLDIST.min <- aggregate(DISTkm ~ CityID, ALLDIST, function(x) min(x))
ALLDIST$lon <- NULL
ALLDIST$lat.x  <- NULL    
ALLDIST$long <- NULL
ALLDIST$lat.y <- NULL
ALLDIST.fullmin <- merge(ALLDIST.min, ALLDIST, by=c("CityID", "DISTkm"))
ALLDIST.fullmin <- merge(ALLDIST.fullmin, UrbanCenters, by=c("full.name"), all.x=TRUE)
ALLDIST.fullmin["Closest.Urban.Center"] <- ALLDIST.fullmin$"full.name"
ALLDIST.fullmin["WC.UC.dist.km"] <- ALLDIST.fullmin$"DISTkm"

#Bringing information on closest Urban Center and the respective distance back into 'world.cities1'
UC.WC.merger <- subset(ALLDIST.fullmin, select = c("CityID", "Closest.Urban.Center", "Population", "WC.UC.dist.km", 
                                                   "Area", "Density", "coastalMC", "largest.UC"))

#New dataset WC.UC.dist! which stands for a merged dataset including distance and estimate if the respective city is part of an urban center
world.cities1["CityID"] <-rownames(world.cities1)
WC.UC.dist1 <- merge(world.cities1, UC.WC.merger, by="CityID")
WC.UC.dist1$WC.UC.dist.km <- as.numeric(WC.UC.dist1$WC.UC.dist.km) 
WC.UC.dist1$Area <- as.numeric(WC.UC.dist1$Area) 
WC.UC.dist1$capital <- as.numeric(WC.UC.dist1$capital)

#CityID is now not needed anymore.
WC.UC.dist1$CityID <- NULL

#Housekeeping
X<-WC.UC.dist1$name
source('SmallScripts/CleanSpecialCharacters.R')
WC.UC.dist1$name <- X
rm(X)

X<-WC.UC.dist1$country.etc
source('SmallScripts/CleanSpecialCharacters.R')
source('SmallScripts/CountryCleaninginCitySet.R')
WC.UC.dist1$country.etc <- X
rm(X)

#Introducing logical variable that tells us whether a city is part of an Urban Center or close to one.
#We use a calculation that treates the middle of the city as the center of a circle.
WC.UC.dist1["part.of.urban.center"] <- (WC.UC.dist1$WC.UC.dist.km <= (20+(2*(((WC.UC.dist1$Area)/pi)**0.5))))
WC.UC.dist1["in.urban.centers.environment"] <- (WC.UC.dist1$WC.UC.dist.km<=(40+(3*(((WC.UC.dist1$Area)/pi)**0.5))))
WC.UC.dist1 <- WC.UC.dist1[order(WC.UC.dist1$pop, na.last=TRUE) , ]

write.csv(WC.UC.dist1, "Cache/WC.UC.dist.first.half.old.csv")

#remove rest
rm(WCmerge, UCmerge, ALLDIST, ALLDIST.min, ALLDIST.fullmin, UC.WC.merger, world.cities1)

#Renaming columns and select subsets for merging over fake variable to create a matrix 
#with each City X each Urban Center (~ 60.000 Cities X ~ 500 urban Centers) 
UCmerge <- subset(UrbanCenters, select = c("lon", "lat", "full.name"))
UCmerge$fake=1
WCmerge <-subset(world.cities2, select = c("long", "lat"))
WCmerge["CityID"] <- rownames(world.cities2)
WCmerge$fake=1

ALLDIST <-merge(UCmerge, WCmerge, by=c("fake"))

#Finding each distance ( ~ 30 million individual distances will be found )
ALLDIST["DISTkm"] <- gdist(ALLDIST$lon, ALLDIST$lat.x, ALLDIST$long, ALLDIST$lat.y, units = "km",
                           a = 6378137, b = 6356752, verbose = FALSE)

#Reducing to only the closest Urban Center for each and every city, 30 million distances to the ~ 60.000 minimal ones
ALLDIST.min <- aggregate(DISTkm ~ CityID, ALLDIST, function(x) min(x))
ALLDIST$lon <- NULL
ALLDIST$lat.x  <- NULL    
ALLDIST$long <- NULL
ALLDIST$lat.y <- NULL
ALLDIST.fullmin <- merge(ALLDIST.min, ALLDIST, by=c("CityID", "DISTkm"))
ALLDIST.fullmin <- merge(ALLDIST.fullmin, UrbanCenters, by=c("full.name"), all.x=TRUE)
ALLDIST.fullmin["Closest.Urban.Center"] <- ALLDIST.fullmin$"full.name"
ALLDIST.fullmin["WC.UC.dist.km"] <- ALLDIST.fullmin$"DISTkm"

#Bringing information on closest Urban Center and the respective distance back into 'world.cities2'
UC.WC.merger <- subset(ALLDIST.fullmin, select = c("CityID", "Closest.Urban.Center", "Population", "WC.UC.dist.km", 
                                                   "Area", "Density", "coastalMC", "largest.UC"))

#New dataset WC.UC.dist! which stands for a merged dataset including distance and estimate if the respective city is part of an urban center
world.cities2["CityID"] <-rownames(world.cities2)
WC.UC.dist2 <- merge(world.cities2, UC.WC.merger, by="CityID")
WC.UC.dist2$WC.UC.dist.km <- as.numeric(WC.UC.dist2$WC.UC.dist.km) 
WC.UC.dist2$Area <- as.numeric(WC.UC.dist2$Area) 
WC.UC.dist2$capital <- as.numeric(WC.UC.dist2$capital)

#CityID is now not needed anymore.
WC.UC.dist2$CityID <- NULL

#Housekeeping
X<-WC.UC.dist2$name
source('SmallScripts/CleanSpecialCharacters.R')
WC.UC.dist2$name <- X
rm(X)

X<-WC.UC.dist2$country.etc
source('SmallScripts/CleanSpecialCharacters.R')
source('SmallScripts/CountryCleaninginCitySet.R')
WC.UC.dist2$country.etc <- X
rm(X)

#Introducing logical variable that tells us whether a city is part of an Urban Center or close to one.
#We use a calculation that treates the middle of the city as the center of a circle.
WC.UC.dist2["part.of.urban.center"] <- (WC.UC.dist2$WC.UC.dist.km <= (20+(2*(((WC.UC.dist2$Area)/pi)**0.5))))
WC.UC.dist2["in.urban.centers.environment"] <- (WC.UC.dist2$WC.UC.dist.km<=(40+(3*(((WC.UC.dist2$Area)/pi)**0.5))))
WC.UC.dist2 <- WC.UC.dist2[order(-WC.UC.dist2$pop, na.last=TRUE) , ]

write.csv(WC.UC.dist2, "Cache/WC.UC.dist.second.half.old.csv")

rm(WCmerge, UCmerge, ALLDIST, ALLDIST.min, ALLDIST.fullmin, UC.WC.merger, world.cities2)

WC.UC.dist <- merge(WC.UC.dist1, WC.UC.dist2, by=c("merge", "name", "country.etc", "pop", "lat", "long", "capital",
                                                   "Region", "Closest.Urban.Center", "Population", "WC.UC.dist.km",
                                                   "Area", "Density", "coastalMC", "largest.UC", "part.of.urban.center",
                                                   "in.urban.centers.environment"), all.x=T, all.y=T)
WC.UC.dist$X.x <- NULL
WC.UC.dist$X.y <- NULL

#remove the two halfs
rm(WC.UC.dist1, WC.UC.dist2)

# Assigning a 1 to all the largest cities in a country
LC <- aggregate(pop ~ country.etc,WC.UC.dist,max)
LC["largestC"] <- 1
WC.UC.dist <- merge(WC.UC.dist, LC, by=c("pop", "country.etc"), all.x=TRUE)
WC.UC.dist <- WC.UC.dist[order(-WC.UC.dist$pop), ]
WC.UC.dist$largestC[is.na(WC.UC.dist$largestC)] <-0
WC.UC.dist$largestC[WC.UC.dist$name == "bombay" &  WC.UC.dist$country.etc  == "india"] <- 1
WC.UC.dist$largestC[WC.UC.dist$name == "mexicocity" &  WC.UC.dist$country.etc  == "mexico"] <- 1
rm(LC)

write.csv(WC.UC.dist, "Cache/WC.UC.dist.old.csv")

