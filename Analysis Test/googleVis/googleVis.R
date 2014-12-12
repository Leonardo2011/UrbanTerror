

# Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("TerrorData/Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]


### take out what we need
Stat2GTD <- subset(PreGTD, select=c(eventid, iyear, country_txt, region_txt, pop.that.year, Rel.CS, inUC, aroundUC, Rank01.C, 
                                    Rank01.W, capital, largestC, largest.UC, WC.UC.dist.km, TUPscale, PROPscale, HUMscale, 
                                    Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR, coast.dist, coast.dist.MIN, coast.dist.MAX, 
                                    access, access.MAX, light, light.MAX, nldi, nldi.MAX, urbn.cover, city.gdp, gdp.MAX, density, 
                                    density.MAX, density.growth, density.growth.MAX, EN.URB.MCTY.TL.ZS, SP.URB.TOTL.IN.ZS, 
                                    EN.URB.LCTY.UR.ZS, latitude, longitude, Pop.Coast.Dist, WCUC.city, WCUC.city.old, merge))


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



##################### Map 0: World during peace ##################### 

GVisD <-Stat2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & Extra.WAR.In==0 & Inter.WAR==0 & Intra.WAR==0)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=1000)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]

GVisD["Population"] <- GVisD$pop.that.year
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
world1 <- gvisGeoChart(GVisD, 
                                 locationvar='Latlong',
                                 sizevar = "Victims", 
                                 colorvar = "Population", 
                                 hovervar="WCUC.city",
                                  options=list(colorAxis="{colors:['green', 'darkred']}",
                                               legend="{textStyle: {color: 'black', fontSize: 16}}",
                                               legend.numberFormat="{numberFormat:'.'}",
                                  backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
                                  width=1112, height=694, markerOpacity=0.4))
plot(world1)

cat(world1$html$chart, file="Analysis Test/googleVis/WorldATpeace_min1000Victims.html")


##################### Map 0B: World during peace before 1990 ##################### 

GVisD <-Stat2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & Extra.WAR.In==0 & Inter.WAR==0 & Intra.WAR==0 & iyear<=1990)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=500)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]

GVisD["Population"] <- GVisD$pop.that.year
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
world2 <- gvisGeoChart(GVisD, 
                      locationvar='Latlong',
                      sizevar = "Victims", 
                      colorvar = "Population", 
                      hovervar="WCUC.city",
                      options=list(colorAxis="{colors:['green', 'red']}",
                                   legend="{textStyle: {color: 'black', fontSize: 16}}",
                                   legend.numberFormat="{numberFormat:'.'}",
                                   backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
                                   width=1112, height=694, markerOpacity=0.4))
plot(world2)

cat(world2$html$chart, file="Analysis Test/googleVis/WorldATpeace_min500Victims_before90.html")



##################### Map 0C: World during peace afrer 1990 ##################### 

GVisD <-Stat2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & Extra.WAR.In==0 & Inter.WAR==0 & Intra.WAR==0 & iyear>=1991)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=500)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]

GVisD["Population"] <- GVisD$pop.that.year
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
world3 <- gvisGeoChart(GVisD, 
                      locationvar='Latlong',
                      sizevar = "Victims", 
                      colorvar = "Population",  
                      hovervar="WCUC.city",
                      options=list(colorAxis="{colors:['green', 'orange']}",
                                   legend="{textStyle: {color: 'black', fontSize: 16}}",
                                   legend.numberFormat="{numberFormat:'.'}",
                                   backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
                                   width=1112, height=694, markerOpacity=0.4))
plot(world3)

cat(world3$html$chart, file="Analysis Test/googleVis/WorldATpeace_min500Victims_after90.html")

##################### Map 0C : World including war before 1990 ##################### 

GVisD <-Stat2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & iyear<=1990)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=500)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]

GVisD["Population"] <- GVisD$pop.that.year
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
world4 <- gvisGeoChart(GVisD, 
                      locationvar='Latlong',
                      sizevar = "Victims", 
                      colorvar = "Population", 
                      hovervar="WCUC.city",
                      options=list(colorAxis="{colors:['green', 'orange']}",
                                   legend="{textStyle: {color: 'black', fontSize: 16}}",
                                   legend.numberFormat="{numberFormat:'.'}",
                                   backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
                                   width=1112, height=694, markerOpacity=0.4))

cat(world4$html$chart, file="Analysis Test/googleVis/World_min500Victims_before90.html")
plot(world4)


##################### Map 0D : World including war after 1990 ##################### 

GVisD <-Stat2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & iyear>=1990)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=500)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]

GVisD["Population"] <- GVisD$pop.that.year
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
world5 <- gvisGeoChart(GVisD, 
                       locationvar='Latlong',
                       sizevar = "Victims", 
                       colorvar = "Population", 
                       hovervar="WCUC.city",
                       options=list(colorAxis="{colors:['green', 'orange']}",
                                    legend="{textStyle: {color: 'black', fontSize: 16}}",
                                    legend.numberFormat="{numberFormat:'.'}",
                                    backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
                                    width=1112, height=694, markerOpacity=0.4))
plot(world5)

cat(world5$html$chart, file="Analysis Test/googleVis/World_min500Victims_after90.html")






##################### Map 1: Region Before 2004  ##################### 

GVisD <-Stat2GTD
GVisD$WCUC.city[GVisD$WCUC.city=="mersin"] <- "mercin"
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15 & iyear<=2003)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=10 & Number.of.Attacks>=5)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]

GVisD["Population"] <- GVisD$pop.that.year
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
regionbefore2004 <- gvisGeoChart(GVisD, 
                          locationvar='Latlong',
                          colorvar = "Victims", 
                          sizevar = "Population",
                          hovervar="WCUC.city",
                          options=list(colorAxis="{colors:['green', 'orange', 'orange', 'darkred']}",
             legend="{textStyle: {color: 'black', fontSize: 16}}",
             legend.numberFormat="{numberFormat:'.'}",
             backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
             width=1112, height=694, markerOpacity=0.5, region=145))

plot(regionbefore2004)

cat(regionbefore2004$html$chart, file="Analysis Test/googleVis/Region_min10Victims_before04.html")

##################### Map 2: Region After 2004  ##################### 

GVisD <-Stat2GTD
GVisD$WCUC.city[GVisD$WCUC.city=="mersin"] <- "mercin"
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15 & iyear>=2004)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=10 & Number.of.Attacks>=5)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]
GVisD["Population"] <- GVisD$pop.that.year
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
GVisD["Population"] <- GVisD$pop.that.year
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
regionafter2004 <- gvisGeoChart(GVisD, 
                                 locationvar='Latlong',
                                 colorvar = "Victims", 
                                 sizevar = "Population",
                                 hovervar="WCUC.city",
                                options=list(colorAxis="{colors:['green', 'orange', 'orange','orange','orange','orange',
                                             'orange','orange','orange','orange','orange','orange','orange','darkred']}",
                                             legend="{textStyle: {color: 'black', fontSize: 16}}",
                                             legend.numberFormat="{numberFormat:'.'}",
                                             backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
                                             width=1112, height=694, markerOpacity=0.5, region=145))
plot(regionafter2004)

cat(regionafter2004$html$chart, file="Analysis Test/googleVis/Region_min10Victims_after04.html")


##################### Map 3: Turkey  ##################### 
GVisD <-Stat2GTD
GVisD$WCUC.city[GVisD$WCUC.city=="mersin"] <- "mercin"
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(longitude) & country_txt=="turkey" & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=1)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]
GVisD["Population"] <- GVisD$pop.that.year
GVisD["Size.1.to.10"] <- round(5*(GVisD$Rank01.C+GVisD$Rel.CS))
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
TurkeyAll <- gvisGeoChart(GVisD, 
                          locationvar='Latlong',
                          colorvar = "Population", 
                          sizevar = "Victims",
                          hovervar="WCUC.city",
                          options=list(region='TR', colorAxis="{colors:['green', 'red', 'red']}",
                                       legend="{textStyle: {color: 'black', fontSize: 16}}",
                                       legend.numberFormat="{numberFormat:'.'}",
                                       backgroundColor="black", dataMode="markers", 
                                       displayMode='text', width=1112, height=694, sizeAxis="{minValue: 60,  maxSize: 70}"))
plot(TurkeyAll)

cat(TurkeyAll$html$chart, file="Analysis Test/googleVis/Turkey.html")


##################### Map 4: Without Istanbul  ##################### 

GVisD <-Stat2GTD
GVisD$WCUC.city[GVisD$WCUC.city=="mersin"] <- "mercin"
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(longitude) & country_txt=="turkey" & WCUC.city!="istanbul" & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15)
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(longitude) & country_txt=="turkey" & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$merge
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=1)
rm(Acount, Ahum)
GVisD <-GVisD[order(-GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]
GVisD["Population"] <- GVisD$pop.that.year
GVisD["Size.1.to.10"] <- round(5*(GVisD$Rank01.C+GVisD$Rel.CS))
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)


TurkeyNOistanbul <- gvisGeoChart(GVisD,
                                 locationvar='Latlong',
                                 colorvar = "Population", 
                                 sizevar = "Victims",
                                 hovervar="WCUC.city",
                                 options=list(region='TR', colorAxis="{colors:['green', 'red', 'red']}",
                                              legend="{textStyle: {color: 'black', fontSize: 16}}",
                                              legend.numberFormat="{numberFormat:'.'}",
                                              backgroundColor="black", dataMode="markers", 
                                              displayMode='text', width=1112, height=694, sizeAxis="{minValue: 20,  maxSize: 50}"))
plot(TurkeyNOistanbul)

cat(TurkeyNOistanbul$html$chart, file="Analysis Test/googleVis/TurkeyNOistanbul.html")
