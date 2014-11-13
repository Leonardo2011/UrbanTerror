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
PreGTD  <- PreGTD [order(-PreGTD$eventid, na.last=TRUE) , ]

PreGTD <- subset(PreGTD, select=c(eventid, merge, iyear, region_txt, city, pop, capital, Closest.Urban.Center, 
                                  CUC.dist.km, part.of.urban.center, in.urban.centers.environment, attacktype1, 
                                  targtype1, targsubtype1, weaptype1, weapsubtype1, TUPscale, PROPscale, HUMscale,
                                  EN.URB.LCTY.UR.ZS, EN.URB.MCTY, EN.URB.MCTY.TL.ZS, SP.URB.GROW, SP.URB.TOTL, 
                                  SP.URB.TOTL.IN.ZS, EN.POP.DNST, EN.RUR.DNST, SP.RUR.TOTL, SP.RUR.TOTL.ZG, SP.RUR.TOTL.ZS))


PreGTD$capital <- recode(PreGTD$capital, "NA=0")

# > sum(is.na(PreGTD$pop))
#[1] 50378         @ 13.11.2014 17:45h

write.csv(PreGTD, file="PreAnalysis/pregtd.csv")
rm(Testframe, GTDcity, GTDcountry, X, Cities, Countries, world.citiesUC)
