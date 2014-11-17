if(file.exists("Cache/WC.UC.dist.csv"))
{UrbanCenters <- read.csv("Cache/WC.UC.dist.csv")
} else
{
  source("CityData/WC.UC.dist.R")
}

if(file.exists("Cache/GTD.csv"))
{world.cities <- read.csv("Cache/GTD.csv")
} else
{
  source("TerrorData/GTD.R")
}
if(file.exists("Cache/WDIData.csv"))
{UrbanCenters <- read.csv("Cache/WDIData.csv")
} else
{
  source("CountryData/WDIData.R")
}


#Interim: Merge GTD and WDI
GTDWDI <-merge(GTD, WDIData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE, sort=TRUE)
#write a pre-gtd



GTDcity <- GTD$city
GTDcountry <- GTD$country_txt
Cities <- WC.UC.dist$name
Countries <- WC.UC.dist$country.etc



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



write.csv(PreGTD, file="PreAnalysis/pregtd.csv")
rm(Testframe, GTDcity, GTDcountry, X, Cities, Countries, world.citiesUC)
