############# URBAN TERROR #############
########################################
########### Part II: Merging ###########
########################################
#######Lukas B Cameron R Sascha S#######
########################################

#In this script we combine all our previously established databases into one, bringing together terror data, country level data
#and city level data into one database ready for analysis.


###### Loading Datasets  ######
(if(file.exists("Cache/GTD.csv"))
{GTD <- read.csv("Cache/GTD.csv")
} else
{
  source("TerrorData/GTD.R")
}
if(file.exists("Cache/WDIData.csv"))
{WDIData <- read.csv("Cache/WDIData.csv")
} else
{
  source("CountryData/WDIData.R")
}
if(file.exists("Cache/WC.UC.dist.csv"))
{WC.UC.dist <- read.csv("Cache/WC.UC.dist.csv")
} else
{
  source("CityData/WC.UC.dist.R")
}

###### GTD & WDI ######

GTDWDI <-merge(GTD, WDIData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE, sort=TRUE)
write.csv(GTDWDI, "Cache/GTDWDI.csv")


##### GTDWDI & City #####
GTDWDIcity <- GTDWDI$city
GTDWDIcountry <- GTDWDI$country_txt
Cities <- WC.UC.dist$name
Countries <- WC.UC.dist$country.etc

WC.UC.dist["merge"] <- paste(Countries, Cities, sep="")
Testframe <- GTDWDI
Testframe["merge"] <-data.frame(paste(GTDWDIcountry, GTDWDIcity, sep=""))

PreGTD <- merge(Testframe, WC.UC.dist, by=c("merge"), all.x=TRUE)
PreGTD  <- PreGTD [order(-PreGTD$eventid, na.last=TRUE) , ]

PreGTD <- subset(PreGTD, select=c(eventid, merge, iyear, region_txt, city, pop, capital, Closest.Urban.Center, 
                                  CUC.dist.km, part.of.urban.center, in.urban.centers.environment, attacktype1, 
                                  targtype1, targsubtype1, weaptype1, weapsubtype1, TUPscale, PROPscale, HUMscale,
                                  EN.URB.LCTY.UR.ZS, EN.URB.MCTY, EN.URB.MCTY.TL.ZS, SP.URB.GROW, SP.URB.TOTL, 
                                  SP.URB.TOTL.IN.ZS, EN.POP.DNST, EN.RUR.DNST, SP.RUR.TOTL, SP.RUR.TOTL.ZG, SP.RUR.TOTL.ZS))


PreGTD$capital <- recode(PreGTD$capital, "NA=0")



write.csv(PreGTD, file="PreAnalysis/pregtd.csv")
rm(Testframe, GTDWDIcity, GTDWDIcountry, X, Cities, Countries, WC.UC.dist, GTDWDI, GTD, WDIData)
