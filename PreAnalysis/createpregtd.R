#testbed to write a pre-gtd

X <- GTD$city
source('CityCleaning.R')
source('SmallScripts/delete_country_special_characters.R')
GTDcity <- X

X <- GTD$country_txt
source('SmallScripts/delete_country_special_characters.R')
GTDcountry <- X

world.cities<-WC.UC.dist
world.cities$CityID <- NULL

X<-world.cities$name
source('SmallScripts/delete_country_special_characters.R')
Cities <- X

X<-world.cities$country.etc
source('SmallScripts/delete_country_special_characters.R')
Countries <- X

world.cities["merge"] <- paste(Countries, Cities, sep="")
Testframe <- GTD[1:21]
Testframe["merge"] <-data.frame(paste(GTDcountry, GTDcity, sep=""))

PreGTD <- merge(Testframe, world.cities, by=c("merge"), all.x=TRUE)
PreGTD  <- PreGTD [order(-PreGTD$HUMscale, na.last=TRUE) , ]


TTT <- subset(PreGTD , select = c("merge", "city", "HUMscale", "name"), na.strings = c("", " "))

PreGTD$iday <- NULL
PreGTD$country <- NULL
PreGTD$region <- NULL
PreGTD$provstate <- NULL
PreGTD$imonth<- NULL
PreGTD$name<- NULL
PreGTD$country.etc<- NULL
PreGTD$lat<- NULL
PreGTD$long<- NULL
PreGTD$Region<- NULL
PreGTD$country.etc<- NULL
write.csv(PreGTD, file="pregtd.csv")
rm(Testframe, t.world.cities, GTDcity, GTDcountry, X, Y)
