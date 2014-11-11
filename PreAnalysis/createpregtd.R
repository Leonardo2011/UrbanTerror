#testbed to write a pre-gtd
X <- GTD$city
source('CityCleaning.R')
X <-gsub("NA", NA, X)
GTDcity <- X
Y <- gsub(" ", "", GTD$country_txt)
Y <- tolower(Y)
Y <-gsub("\\,", "",Y, ignore.case=TRUE)
GTDcountry <- Y
t.world.cities <-world.cities
X<-t.world.cities$name
X <- gsub("\\,.*","",X)
X <- gsub("\\-","",X)
X <- gsub("\\'","",X)
X <- gsub("\\-","",X)
t.world.cities$country.etc<- gsub("\\'","",t.world.cities$country.etc)
t.world.cities$name<-X
world.cities$CityID <- NULL
Testframe <- GTD[1|2|9|11:20]
Testframe["merge"] <-data.frame(paste(GTDcountry, GTDcity, sep=""))
WC09.UCdist$merge <- paste(WC09.UCdist$country.etc, WC09.UCdist$name, sep="")
PreGTD <- merge(Testframe, WC09.UCdist, by=c("merge"), all.x=TRUE)
PreGTD$merge <- NULL
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
