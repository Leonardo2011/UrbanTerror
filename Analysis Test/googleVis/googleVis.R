

# Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("TerrorData/Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]


### take out what we need
GVIS2GTD <- subset(PreGTD, select=c(iyear, country_txt, pop.that.year, Rel.CS, inUC, aroundUC, Rank01.C, Rank01.W, capital, largestC, largest.UC, 
                                    WC.UC.dist.km, TUPscale, PROPscale, HUMscale, coast.dist, coast.dist.MIN, coast.dist.MAX, 
                                    access, access.MAX, light, light.MAX, nldi, nldi.MAX, urbn.cover, city.gdp, gdp.MAX, density, 
                                    density.MAX, density.growth, density.growth.MAX, latitude, longitude, Pop.Coast.Dist, 
                                    WCUC.city, WCUC.city.old))
GVIS2GTD["sumID"] <- paste(GVIS2GTD$country_txt, GVIS2GTD$WCUC.city, sep="")



##################### Map 0: World during peace ##################### 

GVisD <-GVIS2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & Extra.WAR.In==0 & Inter.WAR==0 & Intra.WAR==0)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
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

GVisD["Population"] <- round(GVisD$pop.that.year)
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
                                  width=556, height=343, markerOpacity=0.4))
plot(world1)

cat(world1$html$chart, file="Analysis Test/googleVis/WorldATpeace_min1000Victims.html")


##################### Map 0B: World during peace before 1990 ##################### 

GVisD <-GVIS2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & Extra.WAR.In==0 & Inter.WAR==0 & Intra.WAR==0 & iyear<=1990)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
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

GVisD["Population"] <- round(GVisD$pop.that.year)
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
                                   width=556, height=343, markerOpacity=0.4))
plot(world2)

cat(world2$html$chart, file="Analysis Test/googleVis/WorldATpeace_min500Victims_before90.html")



##################### Map 0C: World during peace afrer 1990 ##################### 

GVisD <-GVIS2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & Extra.WAR.In==0 & Inter.WAR==0 & Intra.WAR==0 & iyear>=1991)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
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

GVisD["Population"] <- round(GVisD$pop.that.year)
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
                                   width=556, height=343, markerOpacity=0.4))
plot(world3)

cat(world3$html$chart, file="Analysis Test/googleVis/WorldATpeace_min500Victims_after90.html")

##################### Map 0C : World including war before 1990 ##################### 

GVisD <-GVIS2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & iyear<=1990)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
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

GVisD["Population"] <- round(GVisD$pop.that.year)
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
                                   width=556, height=343, markerOpacity=0.4))

cat(world4$html$chart, file="Analysis Test/googleVis/World_min500Victims_before90.html")
plot(world4)


##################### Map 0D : World including war after 1990 ##################### 

GVisD <-GVIS2GTD
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & iyear>=1990)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
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

GVisD["Population"] <- round(GVisD$pop.that.year)
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
                                    width=556, height=343, markerOpacity=0.4))
plot(world5)

cat(world5$html$chart, file="Analysis Test/googleVis/World_min500Victims_after90.html")






##################### Map 1: Region Before 2004  ##################### 

GVisD <-GVIS2GTD
GVisD$WCUC.city[GVisD$WCUC.city=="mersin"] <- "mercin"
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15 & iyear<=2003)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=10 & Number.of.Attacks>=3)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]

GVisD["Population"] <- round(GVisD$pop.that.year)
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
regionbefore2004 <- gvisGeoChart(GVisD, 
                          locationvar='Latlong',
                          colorvar = "Victims", 
                          sizevar = "Population",
                          hovervar="WCUC.city",
                          options=list(colorAxis="{colors:['green', 'orange', 'orange', 'red']}",
             legend="{textStyle: {color: 'black', fontSize: 16}}",
             legend.numberFormat="{numberFormat:'.'}",
             backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
             width=667, height=412, markerOpacity=0.7, region=145))

plot(regionbefore2004)

cat(regionbefore2004$html$chart, file="Analysis Test/googleVis/Region_min10Victims_before04.html")

##################### Map 2: Region After 2004  ##################### 

GVisD <-GVIS2GTD
GVisD$WCUC.city[GVisD$WCUC.city=="mersin"] <- "mercin"
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(pop.that.year) & !is.na(longitude) & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15 & iyear>=2004)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
GVisD["one"] <- 1
Acount <- aggregate(one ~ citycode,GVisD,FUN=sum)
Ahum <- aggregate(HUMscale ~ citycode,GVisD,sum)
colnames(Acount)[colnames(Acount) == "one"] <- "Number.of.Attacks"
colnames(Ahum)[colnames(Ahum) == "HUMscale"] <- "Total.Number.of.Victims"
GVisD <- merge(GVisD, Acount, by=c("citycode"), all.x=TRUE)
GVisD <- merge(GVisD, Ahum, by=c("citycode"), all.x=TRUE)
GVisD <- subset(GVisD, Total.Number.of.Victims>=10 & Number.of.Attacks>=3)
rm(Acount, Ahum)
GVisD <-GVisD[order(GVisD$WC.UC.dist.km, -GVisD$pop.that.year, na.last=TRUE) , ]
GVisD <- GVisD[!duplicated(GVisD$WCUC.city),]
GVisD["Population"] <- round(GVisD$pop.that.year)
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
GVisD <-GVisD[order(GVisD$Victims, na.last=TRUE) , ]

regionafter2004 <- gvisGeoChart(GVisD, 
                                 locationvar='Latlong',
                                 colorvar = "Victims", 
                                 sizevar = "Population",
                                 hovervar="WCUC.city",
                                options=list(colorAxis="{colors:['green' ,'orange' ,'orange' ,'orange' ,'orange' ,'orange' ,'orange','orange','orange',
                                             'red' ,'red' ,'red' ,'red' ,'red' ,'red' ,'red','red','red',
                                             'red' ,'red' ,'red' ,'red' ,'red' ,'red' ,'red','red','red', 
                                             'red' ,'red' ,'red' ,'red' ,'red' ,'red' ,'red','red','red', 
                                             'red' ,'red' ,'red' ,'red' ,'red' ,'red' ,'red','red','red', 
                                             'red' ,'red' ,'red' ,'red' ,'red' ,'red' ,'red','red','red', 
                                             'red' ,'red' ,'red' ,'red' ,'red' ,'red' ,'red','red','red', 
                                             'red' ,'red' ,'red' ,'red' ,'red' ,'red' ,'red']}",
                                             legend="{textStyle: {color: 'black', fontSize: 16}}",
                                             legend.numberFormat="{numberFormat:'.'}",
                                             backgroundColor="black", sizeAxis="{minValue: 1,  maxSize: 25}", 
                                             width=667, height=412, markerOpacity=0.7, region=145))
plot(regionafter2004)

cat(regionafter2004$html$chart, file="Analysis Test/googleVis/Region_min10Victims_after04.html")


##################### Map 3: Turkey  ##################### 
GVisD <-GVIS2GTD
GVisD$WCUC.city[GVisD$WCUC.city=="mersin"] <- "mercin"
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(longitude) & country_txt=="turkey" & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
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
GVisD["Population"] <- round(GVisD$pop.that.year)
GVisD["Size.1.to.10"] <- round(5*(GVisD$Rank01.C+GVisD$Rel.CS))
GVisD["Victims"] <- round(GVisD$Total.Number.of.Victim)
TurkeyAll <- gvisGeoChart(GVisD, 
                          locationvar='Latlong',
                          colorvar = "Population", 
                          sizevar = "Victims",
                          hovervar="WCUC.city",
                          options=list(region='TR', colorAxis="{colors:['green', 'red', 'red' 'red', 'red', 'red', 'red', 'red']}",
                                       legend="{textStyle: {color: 'black', fontSize: 16}}",
                                       legend.numberFormat="{numberFormat:'.'}",
                                       backgroundColor="black", dataMode="markers", 
                                       displayMode='text', width=556, height=343, sizeAxis="{minValue: 60,  maxSize: 70}"))
plot(TurkeyAll)

cat(TurkeyAll$html$chart, file="Analysis Test/googleVis/Turkey.html")


##################### Map 4: Without Istanbul  ##################### 

GVisD <-GVIS2GTD
GVisD$WCUC.city[GVisD$WCUC.city=="mersin"] <- "mercin"
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(longitude) & country_txt=="turkey" & WCUC.city!="istanbul" & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15)
GVisD <- subset(GVisD, !is.na(latitude) & !is.na(longitude) & country_txt=="turkey" & longitude<=70 & longitude>=10 & latitude<=45 & latitude>=-15)
GVisD["Latlong"] <- paste(GVisD$latitude, GVisD$longitude, sep=":")
GVisD["citycode"] <- GVisD$sumID
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
GVisD["Population"] <- round(GVisD$pop.that.year)
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
                                              displayMode='text', width=556, height=343, sizeAxis="{minValue: 20,  maxSize: 50}"))
plot(TurkeyNOistanbul)

cat(TurkeyNOistanbul$html$chart, file="Analysis Test/googleVis/TurkeyNOistanbul.html")
