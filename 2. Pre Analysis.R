



#Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("TerrorData/Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]


# take out what we need
Stat1GTD <- subset(PreGTD, select=c(eventid, iyear, country_txt, region_txt, pop.that.year, Rel.CS, inUC, aroundUC, Rank01.C, 
                                  Rank01.W, capital, largestC, largest.UC, WC.UC.dist.km, TUPscale, PROPscale, HUMscale, 
                                  Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR, coast.dist, coast.dist.MIN, coast.dist.MAX, 
                                  access, access.MAX, light, light.MAX, nldi, nldi.MAX, urbn.cover, city.gdp, gdp.MAX, density, 
                                  density.MAX, density.growth, density.growth.MAX, EN.URB.MCTY.TL.ZS, SP.URB.TOTL.IN.ZS, 
                                  EN.URB.LCTY.UR.ZS, latitude, longitude,  WCUC.city.old), iyear>=1998 & iyear<=2011)


# In order to build our varibales, we should exclude the influence of negative components, if there are any
nonnegative <- function(x) ifelse(x<0, 0, x)


### First Dependent Variable: >Urban< 
# 25% (Country relative) City Rank -> Urban as Log Size                             (time variant)
# 25% (Country relative) City Population -> Urban as Size                           (time variant)
# 25% (Country relative) Location's Urban Landcover -> Urban as Surface             (not time variant)
# 25% (Country relative) Location's Nightlights -> Urban as Life                    (not time variant, but data is out there)
Stat1GTD["DV.Target.Urban"] <- (nonnegative(Stat1GTD$Rank01.C*0.25)  
                                + nonnegative(Stat1GTD$Rel.CS*0.25) 
                                + nonnegative(Stat1GTD$urbn.cover/100*0.25)
                                + nonnegative(Stat1GTD$light/Stat1GTD$light.MAX*0.25))

### Second Dependent Variable: >Crowded< 
# 60% (Country relative) Location's Population Density -> Crowded as Density                        (time variant)
# 30% (Country relative) Location's Population Density Growth over past years -> Crowded as Growth  (time variant)
# 10% (Country relative) Night Light Development Index (NLDI) -> Crowded as Inequality              (not time variant)
Stat1GTD["DV.Target.Crowded"] <- (nonnegative(Stat1GTD$density/Stat1GTD$density.MAX*0.60) 
                                  + nonnegative(Stat1GTD$density.growth/Stat1GTD$density.growth.MAX*0.30) 
                                  + nonnegative(Stat1GTD$nldi/Stat1GTD$nldi.MAX*0.10))

### Third Dependent Variable: >Connected< 
# 50% (Country relative) Travel Distance to larger City -> Connected as Proximity to Larger Cities  (not time variant)
# 25% (Country relative) Travel Location's Nightlights -> Connected as Electrified                  (not time variant)
# 25% (Country relative) Locations GDP -> Connected as Productive                                   (not time variant)
Stat1GTD["DV.Target.Connected"] <- (nonnegative((((Stat1GTD$access/Stat1GTD$access.MAX)-1)*-1)*0.5) 
                                    + nonnegative(Stat1GTD$light/Stat1GTD$light.MAX*0.25) 
                                    + nonnegative(Stat1GTD$city.gdp/Stat1GTD$gdp.MAX*0.25))

### Forth Dependent Variable: >Coastal< 
# 100% (Country relative) Distance to the Coast, whereas landlocked countries are NA                (not time variant)
Stat1GTD["DV.Target.Coastal"] <- ifelse(Stat1GTD$coast.dist.MIN >= 30, NA, 
                                        nonnegative(((Stat1GTD$coast.dist/Stat1GTD$coast.dist.MAX)-1)*-1))
rm(nonnegative)

# Now we take aggregated means (country-years), not without putting a weight on the aggregation (Victims^04 + Damage^02))                              
DT <- data.table(Stat1GTD)
DVurb <- data.frame(DT[,list(DV.Target.Urban = weighted.mean(DV.Target.Urban, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcro <- data.frame(DT[,list(DV.Target.Crowded = weighted.mean(DV.Target.Crowded, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcoa <- data.frame(DT[,list(DV.Target.Coastal = weighted.mean(DV.Target.Coastal, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcon <- data.frame(DT[,list(DV.Target.Connected = weighted.mean(DV.Target.Connected, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
Dependent <- merge(DVurb, DVcro, by=c("country_txt", "iyear"), all=TRUE)
Dependent <- merge(Dependent, DVcoa, by=c("country_txt", "iyear"), all=TRUE)
Dependent <- merge(Dependent, DVcon, by=c("country_txt", "iyear"), all=TRUE)


rm(DT, DVurb, DVcro, DVcoa, DVcon)


#YYY <- subset(Stat1GTD, longitude >=27.9 & longitude <= 34.9 & latitude >= 36.2 & latitude <=41.7)
#YYY <- YYY[order(-YYY$HUMscale, na.last=TRUE) , ]

#write.csv(YYY, file="turkytile.csv")

#coplot(DV.Target.Urban ~ iyear|country_txt, type="l", Dependent)
