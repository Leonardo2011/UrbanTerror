#testbed to write a pre-gtd

X <- GTD$city
source('CityCleaning.R')
source('SmallScripts/delete_country_special_characters.R')
GTDcity <- X

X <- GTD$country_txt
source('SmallScripts/delete_country_special_characters.R')
GTDcountry <- X

world.citiesUC<-WC.UC.dist
world.citiesUC$CityID <- NULL

X<-world.citiesUC$name
source('SmallScripts/delete_country_special_characters.R')
Cities <- X

X<-world.citiesUC$country.etc
source('SmallScripts/delete_country_special_characters.R')
Countries <- X

world.citiesUC["merge"] <- paste(Countries, Cities, sep="")
Testframe <- GTD
Testframe["merge"] <-data.frame(paste(GTDcountry, GTDcity, sep=""))

PreGTD <- merge(Testframe, world.citiesUC, by=c("merge"), all.x=TRUE)
PreGTD  <- PreGTD [order(-PreGTD$nkill, na.last=TRUE) , ]




#PreGTD$iday <- NULL
#PreGTD$country <- NULL
#PreGTD$region <- NULL
#PreGTD$provstate <- NULL
#PreGTD$imonth<- NULL
#PreGTD$name<- NULL
#PreGTD$country.etc<- NULL
#PreGTD$lat<- NULL
#PreGTD$long<- NULL
#PreGTD$Region<- NULL
#PreGTD$country.etc<- NULL
#write.csv(PreGTD, file="pregtd.csv")
rm(Testframe, t.world.cities, GTDcity, GTDcountry, X, Y, Cities, Countries)
