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
####### Gathering, Cleaning, Merging #########
##############################################

# In this script we combine all our previously established databases into one, bringing together 
# terror data, country level data and city level data into one database ready for analysis.

# Load Packages if necessary
source("0 - Loading Packages.R")

#############################################################################################################
################### Loading Datasets previously gathered, cleaned and partially merged ######################
#############################################################################################################

# Global Terrorism Database (GTD) with some new scales and categories we introduced to the GTD for our purposes
if(file.exists("Cache/GTD.csv")) {GTD <- read.csv("Cache/GTD.csv")} else{source("1.a - Global Terrorism Database.R")}


# Country level data from the World Bank Development Indicators (WDI) and the The Correlates of War (COW) project data on wars.
if(file.exists("Cache/CountryData.csv")) {CountryData <- read.csv("Cache/CountryData.csv")} else{source("1.b - Country Data.R")}


# City level data from a number of sources, including web scraping and GIS analysis
if(file.exists("Cache/WC.UC.dist.gis.csv")) {WC.UC.dist <- read.csv("Cache/WC.UC.dist.gis.csv")} else{source("1.c - City Data.R")}



#############################################################################################################
############################# Merge the Data Sets into a "PreGTD"###########################################
#############################################################################################################


###### Merge City and Country Data ######

# create merge variable
WC.UC.dist["merge"] <- paste(WC.UC.dist$country.etc, WC.UC.dist$old.name, sep="")
WC.UC.dist <- WC.UC.dist[order(WC.UC.dist$merge, WC.UC.dist$capital, -WC.UC.dist$pop),]
WC.UC.dist <- WC.UC.dist[!duplicated(WC.UC.dist$merge), ]
X <- WC.UC.dist

# create all missing years in the city data artificially
X["start"] <- 1970
X["end"] <- 2013
X <- TimeFill(X, GroupVar = 'merge', StartVar = 'start', EndVar = 'end')
X$TimeFilled <- NULL

# merge city and country data 
WC.UC.full<- merge(X, WC.UC.dist, by=c("merge"), all.x=TRUE)
WC.UC.full <- merge(WC.UC.full, CountryData, by.x=c("country.etc", "Time"), by.y=c("country", "year"), all.x=TRUE, sort=TRUE)
rm(X)

#loosing some weight
WC.UC.full$X.1 <- NULL
WC.UC.full$X.x <- NULL
WC.UC.full$Region <- NULL
WC.UC.full$X.x <- NULL
WC.UC.full$X.y <- NULL
WC.UC.full$EN.RUR.DNST <- NULL
WC.UC.full$SP.RUR.TOTL <- NULL
WC.UC.full$SP.RUR.TOTL.ZG <- NULL
WC.UC.full$SP.RUR.TOTL.ZS <- NULL
WC.UC.full$EN.POP.DNST <- NULL
WC.UC.full$SP.URB.GROW <- NULL

# minor cleanups 
WC.UC.full$part.of.urban.center[is.na(WC.UC.full$part.of.urban.center)] <- FALSE
WC.UC.full$in.urban.centers.environment[is.na(WC.UC.full$in.urban.centers.environment)] <- FALSE
WC.UC.full$in.urban.centers.environment <- recode(WC.UC.full$in.urban.centers.environment, "TRUE=1")
WC.UC.full$part.of.urban.center <- recode(WC.UC.full$part.of.urban.center, "TRUE=1")
WC.UC.full$largestC[is.na(WC.UC.full$largestC)] <- 0 
WC.UC.full$largest.UC[is.na(WC.UC.full$largest.UC)] <- 0 



###### Change City Size on yearly basis with WDI data and introduce relative city size (Rel.CS) ######

# prepare
G2<-WC.UC.full
G2$SP.URB.TOTL <- as.numeric(G2$SP.URB.TOTL )
G2$MAX.URB.TOTL <- as.numeric(G2$MAX.URB.TOTL)
G2$EN.URB.MCTY <- as.numeric(G2$EN.URB.MCTY)
G2$MAX.URB.MCTY <- as.numeric(G2$MAX.URB.MCTY)
G2$EN.URB.LCTY.UR <- as.numeric(G2$EN.URB.LCTY.UR)
G2$MAX.URB.LCTY.UR <- as.numeric(G2$MAX.URB.LCTY.UR)
G2$WC.UC.dist.km <- as.numeric(G2$WC.UC.dist.km)
G2$Area <- as.numeric(G2$Area)
G2["year"] <- as.numeric(G2$Time)
G2["pop.2013"] <- as.numeric(G2$pop)
G2$old.pop <- G2$pop
G2$pop <- NULL


# Area Manipulation for UC's, in order to account for growing urban centers incorporating less area in the past
G2$Area <-ifelse(G2$largest.UC==1 & !is.na(G2$EN.URB.LCTY.UR) & !is.na(G2$MAX.URB.LCTY.UR)  & (G2$EN.URB.LCTY.UR/G2$MAX.URB.LCTY.UR)<=1 & 
                   (G2$EN.URB.LCTY.UR/G2$MAX.URB.LCTY.UR)>=0.05, (G2$EN.URB.LCTY.UR/G2$MAX.URB.LCTY.UR*G2$Area), G2$Area)
G2$Area <-ifelse(G2$largest.UC==0 & (((G2$EN.URB.MCTY - G2$EN.URB.LCTY.UR)/(G2$MAX.URB.MCTY - G2$MAX.URB.LCTY.UR))<=1)& 
                   (((G2$EN.URB.MCTY - G2$EN.URB.LCTY.UR)/(G2$MAX.URB.MCTY - G2$MAX.URB.LCTY.UR))>=0.05) &
                   !is.na((G2$EN.URB.MCTY - G2$EN.URB.LCTY.UR)/(G2$MAX.URB.MCTY - G2$MAX.URB.LCTY.UR)),
                 ((G2$EN.URB.MCTY - G2$EN.URB.LCTY.UR)/(G2$MAX.URB.MCTY - G2$MAX.URB.LCTY.UR)*G2$Area), G2$Area)


# re-answering the question again, if a city is part of an UC, now with new area estimates of all UCs
G2["inUC"] <- ifelse((G2$WC.UC.dist.km <= (15+(((G2$Area)/pi)**0.5))), 1, 0) # 15km + radius of UC as circle
G2["aroundUC"] <- ifelse((G2$WC.UC.dist.km <= (30+(((G2$Area)/pi)**0.5))), 1, 0) # 30km + radius of UC as circle
G2$inUC[is.na(G2$inUC)]<- 0 
G2$name <- ifelse((G2$inUC==1), as.character(G2$name), as.character(G2$old.name))

#in case we only have very limited numers on the country population, we put in some first assumptions based on total population
# and UC population
G2["city.population_with_time"] <- ifelse(G2$inUC==1, G2$Population, G2$pop.2013)
G2$city.population_with_time <- ifelse(!is.na(G2$SP.POP.TOTL), G2$pop.2013*G2$SP.POP.TOTL/G2$MAX.POP.TOTL, G2$city.population_with_time)

#in case we only have URB.POP numbers, we assume that all cities grew with those numbers each year
G2$city.population_with_time <- ifelse(!is.na(G2$SP.URB.TOTL), G2$pop.2013*G2$SP.URB.TOTL/G2$MAX.URB.TOTL, G2$city.population_with_time)

# if it is the largest city, EN.URB.LCTY.UR is the size estimator for each year
G2$city.population_with_time <-ifelse((G2$inUC==0 & G2$largestC==1 & !is.na(G2$EN.URB.LCTY.UR))|
                                        (G2$inUC==1 & G2$largest.UC==1 & !is.na(G2$EN.URB.LCTY.UR)),
                                      G2$EN.URB.LCTY.UR, G2$city.population_with_time)

# if it is a city with less than 1mil, SP.URB.TOTL minus EN.URB.MCTY is the size estimator for each year
G2$city.population_with_time <- ifelse(G2$city.population_with_time!=G2$EN.URB.LCTY.UR
                                       & G2$pop.2013<=999999
                                       & G2$EN.URB.MCTY <= G2$SP.URB.TOTL & G2$MAX.URB.MCTY <= G2$MAX.URB.TOTL
                                       & !(((G2$SP.URB.TOTL-G2$EN.URB.MCTY)/(G2$MAX.URB.TOTL-G2$MAX.URB.MCTY)
                                            *G2$city.population_with_time)>=G2$EN.URB.LCTY.UR)
                                       & !(((G2$SP.URB.TOTL-G2$EN.URB.MCTY)/(G2$MAX.URB.TOTL-G2$MAX.URB.MCTY))<=0)
                                       & !is.na(G2$EN.URB.MCTY) & G2$MAX.URB.TOTL!=0 & G2$MAX.URB.MCTY!=0
                                       & !is.na(G2$EN.URB.LCTY.UR) & !is.na(G2$SP.URB.TOTL),
                                       ((G2$SP.URB.TOTL-G2$EN.URB.MCTY)/(G2$MAX.URB.TOTL-G2$MAX.URB.MCTY)
                                        *G2$city.population_with_time), (G2$city.population_with_time))

# if it is a city with more than 1mil, EN.URB.MCTY minus EN.URB.LCTY.UR is the size estimator for each year
G2$city.population_with_time <- ifelse(G2$city.population_with_time!=G2$EN.URB.LCTY.UR & G2$pop.2013>=999999
                                       & !(((G2$EN.URB.MCTY-G2$EN.URB.LCTY.UR)/
                                              (G2$MAX.URB.MCTY-G2$MAX.URB.LCTY.UR))<=0)
                                       & !(((G2$EN.URB.MCTY-G2$EN.URB.LCTY.UR)/
                                              (G2$MAX.URB.MCTY-G2$MAX.URB.LCTY.UR)
                                            *G2$city.population_with_time)>=G2$EN.URB.LCTY.UR)
                                       & !is.na(G2$EN.URB.MCTY) & !is.na(G2$EN.URB.LCTY.UR) & G2$MAX.URB.MCTY!=0, 
                                       ((G2$EN.URB.MCTY-G2$EN.URB.LCTY.UR)/(G2$MAX.URB.MCTY-G2$MAX.URB.LCTY.UR)
                                        *G2$city.population_with_time), G2$city.population_with_time)

# some final cleaning of minor leftovers
G2$city.population_with_time <- ifelse(!G2$city.population_with_time<=G2$EN.URB.LCTY.UR & G2$largestC==1, 
                                       G2$EN.URB.LCTY.UR, G2$city.population_with_time)
G2$city.population_with_time <- ifelse(!G2$city.population_with_time<=G2$EN.URB.LCTY.UR & G2$largestC==0, 
                                       runif(1, (G2$EN.URB.LCTY.UR/10), G2$EN.URB.LCTY.UR), G2$city.population_with_time)


###### introduce some new variable, because finally we can ######


# introducing relative city size to countries largest city
G2["Rel.CS"] <- G2$city.population_with_time/G2$EN.URB.LCTY.UR

# rename the new population estimate
G2["pop.that.year"] <-  round(G2$city.population_with_time)

# introducing yearly population size rank of each city within its country into the city data
G2["mergerr"] <-data.frame(paste(G2$country.etc, G2$name, G2$year, sep=""))
GX <- G2
GX <- GX[order(GX$mergerr, GX$capital, -GX$pop.that.year),]
GX <- GX[!duplicated(GX$mergerr), ]
GXX <- GX[order(GX$country.etc, GX$year, -GX$pop.that.year, -GX$long),]
GXX <- subset(GXX, !is.na(country.etc))
GXX["RANK.Country"] <- unlist(with(GXX, tapply(-pop.that.year, list(year, country.etc), function(x) rank(x, ties.method= "min"))))
GXX <- subset(GXX, select=c(mergerr, RANK.Country), row.names=NULL)
G2 <- merge(G2, GXX, by=c("mergerr"), all.x=TRUE)
rm(GXX)

# put the maxium rank per year and country in the GTD
Rank.Country.MAX<-aggregate(G2$RANK.Country, by=list(G2$year, G2$country.etc), FUN=max)
colnames(Rank.Country.MAX)[1] <- "iyear"
colnames(Rank.Country.MAX)[2] <- "country_txt"
colnames(Rank.Country.MAX)[3] <- "Rank.C.MAX"
GTDr <- merge(GTD, Rank.Country.MAX, by=c("iyear", "country_txt"), all.x=TRUE)
rm(Rank.Country.MAX)

# introducing yearly population size rank of each city in the world comparison  
GXX<- GX[order(GX$year, -GX$pop.that.year),]
GXX["RANK.World"] <-unlist(with(GXX, tapply(-pop.that.year, year, function(x) rank(x, ties.method= "min"))))
GXX <- subset(GXX, select=c(mergerr, RANK.World), row.names=NULL)
G2 <- merge(G2, GXX, by=c("mergerr"), all.x=TRUE)
G2$mergerr <- NULL
rm(GXX,GX)

# put the maximum rank per year in the GTD
Rank.World.MAX<-aggregate(G2$RANK.World, by=list(G2$year), FUN=max)
colnames(Rank.World.MAX)[1] <- "iyear"
colnames(Rank.World.MAX)[2] <- "Rank.W.MAX"
GTDr <- merge(GTDr, Rank.World.MAX, by=c("iyear"), all.x=TRUE)
rm(Rank.World.MAX)

WC.UC.full<-G2


###### Break the population density data for 1990, 1995, 2000, 2005 and 2010 down to all years ######

WC.UC.full["n"] <- ifelse(WC.UC.full$Time<=1990, 0, WC.UC.full$Time-1990)

WC.UC.full["density"] <- ifelse(WC.UC.full$n<=5, WC.UC.full$dens.90+(WC.UC.full$n/5*(WC.UC.full$dens.95-WC.UC.full$dens.90)), 
                                ifelse(WC.UC.full$n>=5 & WC.UC.full$n<=10, 
                                       WC.UC.full$dens.95+((WC.UC.full$n-5)/5*(WC.UC.full$dens.00-WC.UC.full$dens.95)), 
                                       ifelse(WC.UC.full$n>=10 & WC.UC.full$n<=15, 
                                              WC.UC.full$dens.00+((WC.UC.full$n-10)/5*(WC.UC.full$dens.05-WC.UC.full$dens.00)),
                                              ifelse(WC.UC.full$n>=15 & WC.UC.full$n<=20, 
                                                     WC.UC.full$dens.05+((WC.UC.full$n-15)/5*(WC.UC.full$dens.10-WC.UC.full$dens.05)),
                                                     ifelse(WC.UC.full$n>=20, 
                                                            WC.UC.full$dens.10+((WC.UC.full$n-20)/5*
                                                                                  ((WC.UC.full$dens.10-WC.UC.full$dens.05)+
                                                                                     ((WC.UC.full$dens.10-WC.UC.full$dens.05)/5))), 0))))) 


WC.UC.full["density.growth"] <- ifelse(!is.na(WC.UC.full$dens.90) & WC.UC.full$dens.90!=0 & !is.na(WC.UC.full$dens.95) & WC.UC.full$n<=5 & WC.UC.full$n>=0, 
                                       (WC.UC.full$dens.95-WC.UC.full$dens.90)/WC.UC.full$dens.90, 0)                                  
WC.UC.full$density.growth <- ifelse(!is.na(WC.UC.full$dens.90) & WC.UC.full$dens.90!=0 & !is.na(WC.UC.full$dens.00) & WC.UC.full$n<=10 & WC.UC.full$n>=5, 
                                    (WC.UC.full$dens.00-WC.UC.full$dens.90)/WC.UC.full$dens.90, WC.UC.full$density.growth)                          
WC.UC.full$density.growth <- ifelse(!is.na(WC.UC.full$dens.95) & WC.UC.full$dens.95!=0 & !is.na(WC.UC.full$dens.05) & WC.UC.full$n<=15 & WC.UC.full$n>=10, 
                                    (WC.UC.full$dens.05-WC.UC.full$dens.95)/WC.UC.full$dens.95, WC.UC.full$density.growth)  
WC.UC.full$density.growth <- ifelse(!is.na(WC.UC.full$dens.00) & WC.UC.full$dens.00!=0 & !is.na(WC.UC.full$dens.10) & WC.UC.full$n>=15, 
                                    (WC.UC.full$dens.10-WC.UC.full$dens.00)/WC.UC.full$dens.00, WC.UC.full$density.growth)  



# VarDrop
WC.UC.full$Extra.WAR.In <- NULL
WC.UC.full$n <- NULL
WC.UC.full$Extra.WAR.Out <- NULL
WC.UC.full$Intra.WAR <- NULL
WC.UC.full$Inter.WAR <- NULL
WC.UC.full$EN.POP.DNST <- NULL
WC.UC.full$EN.RUR.DNST <- NULL
WC.UC.full$SP.RUR.TOTL <- NULL
WC.UC.full$SP.RUR.TOTL.ZG <- NULL
WC.UC.full$SP.RUR.TOTL.ZS <- NULL
WC.UC.full$EN.URB.LCTY.UR <- NULL
WC.UC.full$MAX.URB.TOTL <- NULL
WC.UC.full$MAX.URB.MCTY <- NULL
WC.UC.full$MAX.URB.LCTY.UR <- NULL
WC.UC.full$MAX.POP.TOTL <- NULL
WC.UC.full$iso2c <- NULL
WC.UC.full$EN.URB.LCTY.UR.ZS <- NULL
WC.UC.full$EN.URB.MCTY <- NULL
WC.UC.full$EN.URB.MCTY.TL.ZS <- NULL
WC.UC.full$SP.URB.GROW <- NULL
WC.UC.full$SP.URB.TOTL <- NULL
WC.UC.full$SP.POP.TOTL <- NULL
WC.UC.full$SP.URB.TOTL.IN.ZS <- NULL
WC.UC.full$EN.POP.DNST <- NULL
WC.UC.full$EN.RUR.DNST <- NULL
WC.UC.full$SP.RUR.TOTL <- NULL
WC.UC.full$SP.RUR.TOTL.ZG <- NULL
WC.UC.full$SP.RUR.TOTL.ZS <- NULL
WC.UC.full$EN.URB.LCTY.UR <- NULL
WC.UC.full$nldi.MAX <- NULL
WC.UC.full$nldi.MAX <- NULL

##### Add some Maxima to the GTD #######


# Plus Countries Maximum for the Population Density
R.density.MAX<- ifelse(!is.na(WC.UC.full$density), as.numeric(WC.UC.full$density), 0)
R.density.MAX<-aggregate(R.density.MAX, by=list(WC.UC.full$year, WC.UC.full$country.etc), FUN=max)
colnames(R.density.MAX)[1] <- "iyear"
colnames(R.density.MAX)[2] <- "country_txt"
colnames(R.density.MAX)[3] <- "density.MAX"
GTDr <- merge(GTDr, R.density.MAX, by=c("iyear", "country_txt"), all.x=TRUE)
rm(R.density.MAX)


# find maximum density growth for each year and add
density.growth.MAX<-aggregate(WC.UC.full$density.growth, by=list(WC.UC.full$Time, WC.UC.full$country.etc), FUN=max)
colnames(density.growth.MAX)[1] <- "iyear"
colnames(density.growth.MAX)[2] <- "country_txt"
colnames(density.growth.MAX)[3] <- "density.growth.MAX"
GTDr <- merge(GTDr, density.growth.MAX, by=c("iyear", "country_txt"), all.x=TRUE)
rm(density.growth.MAX)


# Plus Countries Maximum for the Night Light Development Index for 2006
R.NLDI.MAX<- ifelse(!is.na(WC.UC.full$nldi), as.numeric(WC.UC.full$nldi), 0)
R.NLDI.MAX<-aggregate(R.NLDI.MAX, by=list(WC.UC.full$country.etc), FUN=max)
colnames(R.NLDI.MAX)[1] <- "country_txt"
colnames(R.NLDI.MAX)[2] <- "nldi.MAX"
GTDr <- merge(GTDr, R.NLDI.MAX, by=c("country_txt"), all.x=TRUE)
rm(R.NLDI.MAX)


# Plus Countries Maximum for Economic activity in millions of dollars per km2 (2006)
R.GDP.MAX<-aggregate(WC.UC.full$city.gdp, by=list(WC.UC.full$country.etc), FUN=max)
colnames(R.GDP.MAX)[1] <- "country_txt"
colnames(R.GDP.MAX)[2] <- "gdp.MAX"
GTDr <- merge(GTDr, R.GDP.MAX, by=c("country_txt"), all.x=TRUE)
rm(R.GDP.MAX)

# Plus Countries Maximum for Stable light shining at night in mean reflection levels 1-63
R.LIGHT.MAX<- ifelse(!is.na(WC.UC.full$light), as.numeric(WC.UC.full$light), 0)
R.LIGHT.MAX<-aggregate(R.LIGHT.MAX, by=list(WC.UC.full$country.etc), FUN=max)
colnames(R.LIGHT.MAX)[1] <- "country_txt"
colnames(R.LIGHT.MAX)[2] <- "light.MAX"
GTDr <- merge(GTDr, R.LIGHT.MAX, by=c("country_txt"), all.x=TRUE)
rm(R.LIGHT.MAX)

# Plus Countries Maximum for travel time to major city in minutes (2000) 
R.ACCESS.MAX<- ifelse(!is.na(WC.UC.full$access), as.numeric(WC.UC.full$access), 0)
R.ACCESS.MAX<-aggregate(R.ACCESS.MAX, by=list(WC.UC.full$country.etc), FUN=max)
colnames(R.ACCESS.MAX)[1] <- "country_txt"
colnames(R.ACCESS.MAX)[2] <- "access.MAX"
GTDr <- merge(GTDr, R.ACCESS.MAX, by=c("country_txt"), all.x=TRUE)
rm(R.ACCESS.MAX)

# Plus Countries Minimum for Distance to coast in km 
R.COASTDIST.MIN<-aggregate(WC.UC.full$coast.dist, by=list(WC.UC.full$country.etc), FUN=min)
colnames(R.COASTDIST.MIN)[1] <- "country_txt"
colnames(R.COASTDIST.MIN)[2] <- "coast.dist.MIN"
GTDr <- merge(GTDr, R.COASTDIST.MIN, by=c("country_txt"), all.x=TRUE)
rm(R.COASTDIST.MIN)

# Plus Countries Maximum for Distance to coast in km
R.COASTDIST.MAX<-aggregate(WC.UC.full$coast.dist, by=list(WC.UC.full$country.etc), FUN=max)
colnames(R.COASTDIST.MAX)[1] <- "country_txt"
colnames(R.COASTDIST.MAX)[2] <- "coast.dist.MAX"
GTDr <- merge(GTDr, R.COASTDIST.MAX, by=c("country_txt"), all.x=TRUE)
rm(R.COASTDIST.MAX)


# In order to determine population movements, we define the average coastal distance of country populations
DT <- data.table(WC.UC.full)                
Pop.Coast.Dist <- data.frame(DT[,list(Pop.Coast.Dist = weighted.mean(coast.dist, pop.that.year, rm.na=TRUE)),by=list(country.etc, Time)])
GTDr <- merge(GTDr, Pop.Coast.Dist, by.x=c("iyear", "country_txt"), by.y=c("Time", "country.etc"), all.x=TRUE)
WC.UC.full.towns <- subset(WC.UC.full, pop.that.year<= 5000)
DT <- data.table(WC.UC.full.towns)                
Small.Town.Pop.Coast.Dist <- data.frame(DT[,list(Town.Coast.Dist = weighted.mean(coast.dist, pop.that.year, rm.na=TRUE)),by=list(country.etc, Time)])
GTDr <- merge(GTDr, Small.Town.Pop.Coast.Dist, by.x=c("iyear", "country_txt"), by.y=c("Time", "country.etc"), all.x=TRUE)
rm(Pop.Coast.Dist, DT, Small.Town.Pop.Coast.Dist, WC.UC.full.towns)


###### Merge combined set with GTD ######
# merge
GTD2 <- merge(GTDr, CountryData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE, sort=TRUE)
rm(GTDr)

WC.UC.merge <- WC.UC.full$merge
WC.UC.time <- WC.UC.full$Time
WC.UC.full["merge2"] <- paste(WC.UC.merge, WC.UC.time, sep="")
GTDcity <- GTD2$city
GTDcountry <- GTD2$country_txt
GTDyear <-GTD2$iyear
GTD2["merge"] <-data.frame(paste(GTDcountry, GTDcity, sep=""))
GTD2["merge2"] <-data.frame(paste(GTDcountry, GTDcity, GTDyear, sep=""))

PreGTD <- merge(GTD2, WC.UC.full, by=c("merge2"), all.x=TRUE)
PreGTD  <- PreGTD [order(-PreGTD$HUMscale, na.last=TRUE) , ]

# bring the lat lon data together from both the GTD and the city data sets
PreGTD$latitude <- ifelse(is.na(PreGTD$latitude), as.numeric(PreGTD$lat), as.numeric(PreGTD$lat))
PreGTD$longitude <- ifelse(is.na(PreGTD$longitude), as.numeric(PreGTD$long), as.numeric(PreGTD$long))

# now we add a random noise to the latlong data, in case the location was previously identified as non-city 
# geo data. We found a lot of locations on the geonames severes that e.g. is rather a province capital, once we search for
# a province name from the GTD. Those were identified and now a random noise is added in order to bring the latlong outside of 
# city. Just to be sure. 

add.geo.noise <- function (x) {
  z <- runif(1, -1, 1)
  z <- z/abs(z)
  z <- z*runif(1, 0.3, 1)
  x <- z+x
  rm(z)
  return(x)
}

PreGTD$longitude[PreGTD$non.city.loc == 1] <- add.geo.noise(PreGTD$longitude)
PreGTD$latitude[PreGTD$non.city.loc == 1] <- add.geo.noise(PreGTD$latitude)

rm(add.geo.noise)

# Introduce loged ranks 
PreGTD["Rank01.C"] <-((((PreGTD$RANK.Country-1)/(PreGTD$Rank.C.MAX-1))-1)*-1)
PreGTD["Rank01.W"] <-((((PreGTD$RANK.World-1)/(PreGTD$Rank.W.MAX-1))-1)*-1)


# better names before subsetting
PreGTD["GTD.city"] <- PreGTD$city
PreGTD["WCUC.city.old"] <- PreGTD$old.name
PreGTD["WCUC.city"] <- PreGTD$name
PreGTD["merge"]<- PreGTD$merge.x



# limit and order the new PreGTD
PreGTD <- subset(PreGTD, select=c(eventid, iyear, Date, country_txt, GTD.city, WCUC.city.old, WCUC.city,
                                  latitude, longitude, non.city.loc, pop.that.year, Rel.CS, inUC, aroundUC, RANK.Country, Rank.C.MAX, Rank01.C, 
                                  RANK.World, Rank.W.MAX, Rank01.W, capital, largestC, Closest.Urban.Center,largest.UC, 
                                  coastalMC, WC.UC.dist.km, attacktype1, targtype1, targsubtype1,  
                                  TUPscale, PROPscale, HUMscale, Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR, old.pop, 
                                  merge, original.city, coast.dist, Pop.Coast.Dist, Town.Coast.Dist, coast.dist.MIN, coast.dist.MAX, access, access.MAX, light, 
                                  light.MAX, nldi, nldi.MAX, urbn.cover, city.gdp, gdp.MAX, density, density.MAX, density.growth, 
                                  density.growth.MAX, EN.URB.MCTY.TL.ZS, SP.URB.TOTL.IN.ZS, EN.URB.LCTY.UR.ZS))


# fill rural atacks (= no city found) with respective data 
PreGTD$coastalMC[is.na(PreGTD$coastalMC)] <- 0 
PreGTD$capital[is.na(PreGTD$capital)] <- 0 
PreGTD$largest.UC[is.na(PreGTD$largest.UC)] <- 0 
PreGTD$TUPscale[is.na(PreGTD$TUPscale)] <- 0 
PreGTD$PROPscale[is.na(PreGTD$PROPscale)] <- 0 
PreGTD$HUMscale[is.na(PreGTD$HUMscale)] <- 0 
PreGTD$Extra.WAR.In[is.na(PreGTD$Extra.WAR.In)] <- 0 
PreGTD$Extra.WAR.Out[is.na(PreGTD$Extra.WAR.Out)] <- 0 
PreGTD$Intra.WAR[is.na(PreGTD$Intra.WAR)] <- 0 
PreGTD$Inter.WAR[is.na(PreGTD$Inter.WAR)] <- 0 
PreGTD$Rel.CS[is.na(PreGTD$Rel.CS)] <- 0 
PreGTD$Rank01.C[is.na(PreGTD$Rank01.C)] <- 0 
PreGTD$Rank01.W[is.na(PreGTD$Rank01.W)] <- 0 
PreGTD$inUC[is.na(PreGTD$inUC)] <- 0 
PreGTD$aroundUC[is.na(PreGTD$aroundUC)] <- 0 
PreGTD$largestC[is.na(PreGTD$largestC)] <- 0 
PreGTD$urbn.cover[is.na(PreGTD$urbn.cover)] <- 0 
PreGTD$access <- ifelse(is.na(PreGTD$access), as.numeric(PreGTD$access.MAX), as.numeric(PreGTD$access))
PreGTD$light <- ifelse(is.na(PreGTD$light), 0, as.numeric(PreGTD$light))
PreGTD$nldi <- ifelse(is.na(PreGTD$nldi), as.numeric(PreGTD$nldi.MAX), as.numeric(PreGTD$nldi))
PreGTD$density <- ifelse(is.na(PreGTD$density), 0, as.numeric(PreGTD$density))
PreGTD$city.gdp <- ifelse(is.na(PreGTD$city.gdp), 0, as.numeric(PreGTD$city.gdp))
PreGTD$density.growth <- ifelse(is.na(PreGTD$density.growth), 0, PreGTD$density.growth)
PreGTD$RANK.World <- ifelse(is.na(PreGTD$RANK.World), as.numeric(PreGTD$Rank.World.MAX), PreGTD$RANK.World)
PreGTD$RANK.World <- ifelse(is.na(PreGTD$RANK.World), as.numeric(PreGTD$Rank.World.MAX), PreGTD$RANK.World)
PreGTD$EN.URB.LCTY.UR.ZS <- as.numeric(PreGTD$EN.URB.LCTY.UR.ZS)
PreGTD$SP.URB.TOTL.IN.ZS <- as.numeric(PreGTD$SP.URB.TOTL.IN.ZS)
PreGTD$EN.URB.MCTY.TL.ZS <- as.numeric(PreGTD$EN.URB.MCTY.TL.ZS)
PreGTD$coast.dist <- ifelse(is.na(PreGTD$coast.dist), as.numeric(PreGTD$Town.Coast.Dist), as.numeric(PreGTD$coast.dist))
PreGTD$pop <- as.numeric(PreGTD$pop)


######################### 
##### For the sake of later simplicity, we build in the first dependent Variables at this point
# In order to build our Varibales, we should exclude the influence of negative components, if there are any
nonnegative <- function(x) ifelse(x<0, 0, x)

### First Dependent Variable: >Urban< 
# 25% (Country relative) City Rank -> Urban as Log Size                             (time variant)
# 25% (Country relative) City Population -> Urban as Size                           (time variant)
# 25% (Country relative) Location's Urban Landcover -> Urban as Surface             (not time variant)
# 25% (Country relative) Location's Nightlights -> Urban as Life                    (not time variant, but data is out there)
PreGTD["DV.Target.Urban"] <- (nonnegative(PreGTD$Rank01.C*25)  
                                + nonnegative(PreGTD$Rel.CS*25) 
                                + nonnegative(PreGTD$urbn.cover/100*25)
                                + nonnegative(PreGTD$light/PreGTD$light.MAX*25))

### Second Dependent Variable: >Crowded< 
# 60% (Country relative) Location's Population Density -> Crowded as Density                        (time variant)
# 30% (Country relative) Location's Population Density Growth over past years -> Crowded as Growth  (time variant)
# 10% (Country relative) Night Light Development Index (NLDI) -> Crowded as Inequality              (not time variant)
PreGTD["DV.Target.Crowded"] <- (nonnegative(PreGTD$density/PreGTD$density.MAX*60) 
                                  + nonnegative(PreGTD$density.growth/PreGTD$density.growth.MAX*30) 
                                  + nonnegative(PreGTD$nldi/PreGTD$nldi.MAX*10))

### Third Dependent Variable: >Connected< 
# 50% (Country relative) Travel Distance to larger City -> Connected as Proximity to Larger Cities  (not time variant)
# 25% (Country relative) Travel Location's Nightlights -> Connected as Electrified                  (not time variant)
# 25% (Country relative) Locations GDP -> Connected as Productive                                   (not time variant)
PreGTD["DV.Target.Connected"] <- (nonnegative((((PreGTD$access/PreGTD$access.MAX)-1)*-1)*50) 
                                    + nonnegative(PreGTD$light/PreGTD$light.MAX*25) 
                                    + nonnegative(PreGTD$city.gdp/PreGTD$gdp.MAX*25))

### Forth Dependent Variable: >Coastal< 
# 100% (Country relative) Distance to the Coast, whereas landlocked countries are NA                (not time variant)
PreGTD["DV.Target.Coastal"] <- ifelse(PreGTD$coast.dist.MIN >= 20, NA, 
                                        nonnegative(((PreGTD$coast.dist/PreGTD$coast.dist.MAX)-1)*-100))
rm(nonnegative)

### Fifth Dependent Variable: >Kilcullen< 
# The combination of all four variables, or three in case of landlocked countries                (not time variant)

PreGTD["DV.Kilcullen"] <- ifelse(is.na(PreGTD$DV.Target.Coastal), 
                                   ((PreGTD$DV.Target.Urban+PreGTD$DV.Target.Crowded+PreGTD$DV.Target.Connected)/3),
                                   (PreGTD$DV.Target.Urban+PreGTD$DV.Target.Crowded+PreGTD$DV.Target.Connected+PreGTD$DV.Target.Coastal)/4)

###  One additional Dependent Variable: The average populations distance from the coast
DT <- data.table(PreGTD)
IVdst <- data.frame(DT[,list(IV.Pop.Coastal.Dist = mean(Pop.Coast.Dist, rm.na=TRUE)),by=list(country_txt, iyear)])
PreGTD <- merge(PreGTD, IVdst, by=c("country_txt", "iyear"), all.x=TRUE)
PreGTD$IV.Pop.Coastal.Dist[is.na(PreGTD$DV.Target.Coastal)] <- NA
rm(DT, IVdst)


# write a csv, just to be sure
write.csv(PreGTD, file="TerrorData/Pregtd.csv")

# the end
rm(WC.UC.merge, WC.UC.time, GTDcountry, GTDcity, GTDyear, GTD2, WC.UC.full, G2, GTD, WC.UC.dist)
