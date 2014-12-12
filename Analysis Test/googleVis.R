

# Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("TerrorData/Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]


### take out what we need
Stat2GTD <- subset(PreGTD, select=c(eventid, iyear, country_txt, region_txt, pop.that.year, Rel.CS, inUC, aroundUC, Rank01.C, 
                                    Rank01.W, capital, largestC, largest.UC, WC.UC.dist.km, TUPscale, PROPscale, HUMscale, 
                                    Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR, coast.dist, coast.dist.MIN, coast.dist.MAX, 
                                    access, access.MAX, light, light.MAX, nldi, nldi.MAX, urbn.cover, city.gdp, gdp.MAX, density, 
                                    density.MAX, density.growth, density.growth.MAX, EN.URB.MCTY.TL.ZS, SP.URB.TOTL.IN.ZS, 
                                    EN.URB.LCTY.UR.ZS, latitude, longitude, Pop.Coast.Dist, WCUC.city.old, merge))


# In order to build our Varibales, we should exclude the influence of negative components, if there are any
nonnegative <- function(x) ifelse(x<0, 0, x)


### First Dependent Variable: >Urban< 
# 25% (Country relative) City Rank -> Urban as Log Size                             (time variant)
# 25% (Country relative) City Population -> Urban as Size                           (time variant)
# 25% (Country relative) Location's Urban Landcover -> Urban as Surface             (not time variant)
# 25% (Country relative) Location's Nightlights -> Urban as Life                    (not time variant, but data is out there)
Stat2GTD["DV.Target.Urban"] <- (nonnegative(Stat2GTD$Rank01.C*0.25)  
                                + nonnegative(Stat2GTD$Rel.CS*0.25) 
                                + nonnegative(Stat2GTD$urbn.cover/100*0.25)
                                + nonnegative(Stat2GTD$light/Stat2GTD$light.MAX*0.25))

### Second Dependent Variable: >Crowded< 
# 60% (Country relative) Location's Population Density -> Crowded as Density                        (time variant)
# 30% (Country relative) Location's Population Density Growth over past years -> Crowded as Growth  (time variant)
# 10% (Country relative) Night Light Development Index (NLDI) -> Crowded as Inequality              (not time variant)
Stat2GTD["DV.Target.Crowded"] <- (nonnegative(Stat2GTD$density/Stat2GTD$density.MAX*0.60) 
                                  + nonnegative(Stat2GTD$density.growth/Stat2GTD$density.growth.MAX*0.30) 
                                  + nonnegative(Stat2GTD$nldi/Stat2GTD$nldi.MAX*0.10))

### Third Dependent Variable: >Connected< 
# 50% (Country relative) Travel Distance to larger City -> Connected as Proximity to Larger Cities  (not time variant)
# 25% (Country relative) Travel Location's Nightlights -> Connected as Electrified                  (not time variant)
# 25% (Country relative) Locations GDP -> Connected as Productive                                   (not time variant)
Stat2GTD["DV.Target.Connected"] <- (nonnegative((((Stat2GTD$access/Stat2GTD$access.MAX)-1)*-1)*0.5) 
                                    + nonnegative(Stat2GTD$light/Stat2GTD$light.MAX*0.25) 
                                    + nonnegative(Stat2GTD$city.gdp/Stat2GTD$gdp.MAX*0.25))

### Forth Dependent Variable: >Coastal< 
# 100% (Country relative) Distance to the Coast, whereas landlocked countries are NA                (not time variant)
Stat2GTD["DV.Target.Coastal"] <- ifelse(Stat2GTD$coast.dist.MIN >= 20, NA, 
                                        nonnegative(((Stat2GTD$coast.dist/Stat2GTD$coast.dist.MAX)-1)*-1))
rm(nonnegative)



GVisD <-Stat2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(longitude) & country_txt=="turkey" & longitude<=180 & longitude>=-180 & latitude<=90 & latitude>=-90)
GVisD["Latlong"] <- as.character(paste(GVisD$latitude, GVisD$longitude, sep=":"))
GVisD["citycode"] <- GVisD$merge
GVisD["Number.of.Attacks"] <- 1

Acount <- aggregate(Number.of.Attacks ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
rm(Acount, Ahum)
GVisD <- GVisD[!duplicated(GVisD$WCUC.city.old),]

Geo1 <- gvisGeoMap(GVisD, locationvar="Latlong",numvar='Number.of.Victims',
                   hovervar="WCUC.city.old",
                     options=list(region=145, dataMode="markers"))
plot(Geo1)





