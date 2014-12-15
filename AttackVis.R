# This script visualizes our Data Gathering, Merging and Manipulating Process with the example of Turkey
source("0 - Loading Packages.R")
PreGTD <- read.csv("TerrorData/pregtd.csv")

#Finding 2 basic maps of Turkey for visualisation
Turkey <- qmap("Davulga", zoom = 7, extent = "device", legend = "topleft")
BaseMap <- qmap("Davulga", zoom = 7, extent = "device", source="stamen", maptype="toner")


#Creating our PreGTD subset with example attacks
ExA <- subset(PreGTD, eventid == 200608280001 | eventid == 199811270002 | eventid == 199903050002 
                    | eventid == 200308010004 | eventid == 199907300003 | eventid == 199809090001
                    | eventid == 200311140004 )

ExA$inUC <- as.factor(ExA$inUC)
ExA$latitude <- as.numeric(ExA$latitude)
ExA$longitude <- as.numeric(ExA$longitude)


# Attack Summaries:
# 200807270020: On Sunday, two bombs exploded minutes apart in one of Istanbul's busy shopping districts on the European side. Though the Kurdistan Workers Party denied responsibility for the attack, officials continually accuse them of direct involvement. 
# 200608280001: Four people were killed and approximately 65 were injured when a bomb attached to a motorcycle detonated in Antalya, Turkey. No one claimed responsibility for the attack.
# 199811270002: A bomb exploded on a bus near Kirikkale, Turkey. The explosion killed four passengers and injured 20 others. The bomb was placed in the luggage department of the bus. There was no claim of responsibility for this attack.
# 200608280002: A car filled with explosives exploded 80 meters from the Russian Consulate General in Antalya, Turkey, killing three people and wounding at least 20 more. No one claimed responsibility for the attack, but authorities believed the Kurdish separatists were to blame. 
# 199903050002: A car bomb attack targeted the Cankiri Provincial Governor, Ayhan Cevik, while he was driving in Cankiri, Turkey. Four people were killed and the Governor and nine others were wounded. The Turkish Workers and Peasants Liberation Army (TKP/ML-TIKKO) claimed responsibility for the attack
# 200308010004: An unnamed leftist organization detonated an explosive device in the garden of the Justice Ministry's Center of Education for Judge and Prosecutor Candidates in Ankara, Turkey. Eleven people, including policemen and judges, were injured in the explosion. Although a group claimed responsibility for the attack, Turkey's Interior Minister, Abdulkadir Aksu, did not publicly reveal the perpetrator group's name
# 199907300003: A total of four village guards were killed in Gurpinar, Turkey, when a group of perpetrators attacked the guards who were protecting the Telekom employees. Officials reported that the employees where the target of this attack. 
# 199809090001: In one of two related attacks, rebels from the Kurdistan Workers Party (PKK) attacked the Imamli settlement unit in the village of Kuyucak, Turkey, killing one person and injuring three (Mustafa, Hilmi and Nuriye Atici). The perpetrators fled after this incident.
# 200311140004: Two car bombings by the militant Islamic group, Great East Islamic Raiders Front (IBDA-C - Islami Buyuk Dogu Akincilar Cephesi), on synagogues in Istanbul, Turkey, killed at least twenty people and injured 302 others. One of the two almost simultaneous blasts occurred at the Beth Israel Synagogue, damaging the building and several cars. Numerous people were reportedly killed and injured in the bombing. The group identified itself in a telephone call to the Anadolu News Agency. 


# Formatting the table so it does look nicely in Rmarkdwon and represents the right things
Rmdattacks <- subset(ExA, select = c(Date, inUC, HUMscale, access, access.MAX, light, light.MAX, density, density.MAX, coast.dist, coast.dist.MAX, original.city, latitude))
#ordering attacks from North to South
Rmdattacks <- Rmdattacks[order(-Rmdattacks$latitude), ]

Rmdattacks$inUC <- as.character(Rmdattacks$inUC)
Rmdattacks$inUC[Rmdattacks$inUC == 0] <- "no"
Rmdattacks$inUC[Rmdattacks$inUC == 1] <- "yes"

Rmdattacks$Rel.Access <- round((((Rmdattacks$access/Rmdattacks$access.MAX)-1)*-100), digits = 2 )
Rmdattacks$Rel.Light <- round(((Rmdattacks$light/Rmdattacks$light.MAX)*100), digits = 2)
Rmdattacks$Rel.Density <- round(((Rmdattacks$density/Rmdattacks$density.MAX)*100), digits = 2)
Rmdattacks$Rel.Coast.Dist <- round((((Rmdattacks$coast.dist/Rmdattacks$coast.dist.MAX)-1)*-100), digits = 2)
Rmdattacks$Date.Place <- paste(Rmdattacks$Date, Rmdattacks$original.city, sep=", ")
Rmdattacks$latitude <- NULL
Rmdattacks$access <- NULL
Rmdattacks$access.MAX <- NULL
Rmdattacks$density <- NULL
Rmdattacks$density.MAX <- NULL
Rmdattacks$light <- NULL
Rmdattacks$light.MAX <- NULL
Rmdattacks$coast.dist <- NULL
Rmdattacks$coast.dist.MAX <- NULL
Rmdattacks$Date <- NULL
Rmdattacks$original.city <- NULL
colnames(Rmdattacks) <- c("On.UC", "Kill.Wound", "Access", "Light", "Dens", "Prox.Coast", "Date.Place")
Rmdattacks <- Rmdattacks[c(7,1,2,4,3,6,5) ]
Rmdattacks$row.names <- NULL


# show example attacks on map with distinginishing if take place on UC and labels
AttackMap <- BaseMap + geom_point(aes(x=longitude, y=latitude, shape = inUC, color = inUC), data = ExA, size = 7 ) + 
             theme(legend.position="none")+
             geom_text(aes(x=longitude+0.3, y=latitude+0.08, fontface="bold", color = inUC, fontsize = 7), data=ExA, label=ExA$Date) + 
             scale_colour_manual(values = c("firebrick","pink"))

# Show lightmap
unzip("Downloaded_Data/Downloaded_Raster_Data.zip", exdir="Downloaded_Data")
unlink("Downloaded_Data/NLDI_2006_0p25_rev20111230.tif")
unlink("Downloaded_Data/GDP_grid_flt.tif")
unlink("Downloaded_Data/DICGSH1a.tif")
unlink("Downloaded_Data/GACGEM2a.tif")
unlink("Downloaded_Data/G19ESA3a.tif")
RASTERlight<- raster("Downloaded_Data/LNMDMS2a.tif")
#crop Raster to Turkey maps boundaries
e <- extent(27.9, 34.9, 36.2, 41.7)
LightLayer <- crop(RASTERlight, e)

#convert to usable dataframe including eliminating minor light values and aggregating the rest from 63 to only 6 levels
LightLayer <- as(LightLayer, "SpatialPixelsDataFrame")
LightLayer <- data.frame(LightLayer)
LightLayer$y <- LightLayer$y-0.09
colnames(LightLayer) <- c("light", "longitude", "latitude")
LightLayer$light <- as.numeric(LightLayer$light)
#LightLayer <- subset(LightLayer, light >60)
LightLayer$light <- recode (LightLayer$light, "0:4 = 0; 4:10 = 1 ; 11:18 = 2; 19:30 = 3; 31:40 = 4; 41:50 = 5; 51:63 = 6", as.numeric.result=TRUE)

LightMap <- BaseMap +   geom_tile(aes(x = longitude, y = latitude, fill=light), data = LightLayer, alpha = 0.6) +
  geom_point(aes(x=longitude, y=latitude-0.1, color="white"), data = ExA, size = 5 ) +
  geom_point(aes(x=longitude, y=latitude-0.1, color="red"), data = ExA, size = 4 ) +
  geom_point(aes(x=longitude, shape= inUC, color=inUC, y=latitude-0.1), data = ExA, size = 8 ) +
  geom_text(aes(x=longitude+0.3, y=latitude+0.19, fontface="bold", colour="firebrick", fontsize = 7), data=ExA, label=ExA$Date) +
  scale_fill_gradient(low = "darkblue", high= "yellow") + 
  scale_shape_manual(values=c(10, 10, 13, 13)) +
  scale_colour_manual(values = c("white","white", "white","black", "firebrick", "firebrick")) +
  theme(legend.position="none")

unlink("Downloaded_Data/LNMDMS2a.tif")
rm(LightLayer, RASTERlight, e)


# Show Accessmap
unzip("Downloaded_Data/Downloaded_Raster_Data.zip", exdir="Downloaded_Data")
unlink("Downloaded_Data/NLDI_2006_0p25_rev20111230.tif")
unlink("Downloaded_Data/GDP_grid_flt.tif")
unlink("Downloaded_Data/DICGSH1a.tif")
unlink("Downloaded_Data/G19ESA3a.tif")
unlink("Downloaded_Data/LNMDMS2a.tif")
RASTERAccess<- raster("Downloaded_Data/GACGEM2a.tif")
#crop Raster to Turkey maps boundaries
e <- extent(27.9, 34.9, 36.2, 41.7)
AccessLayer <- crop(RASTERAccess, e)
#aggregating cells because else, way too muh values
#AccessLayer <- raster::aggregate(AccessLayer, fact=1, fun=mean, na.rm=TRUE)
#convert to usable dataframe including eliminating largest Access time values (in minutes ) and aggregating the rest to 10 min intervals
AccessLayer <- as(AccessLayer, "SpatialPixelsDataFrame")
AccessLayer <- data.frame(AccessLayer)
AccessLayer$y <- AccessLayer$y-0.085
colnames(AccessLayer) <- c("AccessTime", "longitude", "latitude")
AccessLayer$AccessTime <- as.numeric(AccessLayer$AccessTime)

#subsetting and binning Access Time values to 10 minute bins to reduce observations. N
#ow, only displaying areas that have Access Time < 120 minutes
#AccessLayer <- subset(AccessLayer, AccessTime < 120)
AccessLayer$AccessTime <- AccessLayer$AccessTime/10
AccessLayer$AccessTime <- ifelse(AccessLayer$AccessTime >= 18, 18, AccessLayer$AccessTime)
AccessLayer$AccessTime <- round(AccessLayer$AccessTime)


AccessMap <- BaseMap + geom_tile(aes(x = longitude, y = latitude, fill=AccessTime), data = AccessLayer, alpha = 0.75) + 
  geom_point(aes(x=longitude, y=latitude-0.1, color="white"), data = ExA, size = 5 ) +
  geom_point(aes(x=longitude, y=latitude-0.1, color="red"), data = ExA, size = 4 ) +
  geom_point(aes(x=longitude, shape= inUC, color=inUC, y=latitude-0.1), data = ExA, size = 8 ) +
  geom_text(aes(x=longitude+0.3, y=latitude+0.19, fontface="bold", colour="firebrick", fontsize = 7), data=ExA, label=ExA$Date) +
  scale_fill_gradient(low = "yellow", high= "darkgreen") +
  scale_shape_manual(values=c(10, 10, 13, 13)) +
  scale_colour_manual(values = c("white","white", "white","black", "firebrick", "firebrick")) +
  theme(legend.position="none")

unlink("Downloaded_Data/GACGEM2a.tif")
rm(RASTERAccess, e)


# Proximity to Coast
unzip("Downloaded_Data/Downloaded_Raster_Data.zip", exdir="Downloaded_Data")
unlink("Downloaded_Data/NLDI_2006_0p25_rev20111230.tif")
unlink("Downloaded_Data/GDP_grid_flt.tif")
unlink("Downloaded_Data/GACGEM2a.tif")
unlink("Downloaded_Data/G19ESA3a.tif")
unlink("Downloaded_Data/LNMDMS2a.tif")
RASTERCoast<- raster("Downloaded_Data/DICGSH1a.tif")
#crop Raster to Turkey maps boundaries
e <- extent(27.9, 34.9, 36.2, 41.7)
CoastLayer <- crop(RASTERCoast, e)
CoastLayer <- as(CoastLayer, "SpatialPixelsDataFrame")
CoastLayer <- data.frame(CoastLayer)
CoastLayer$y <- CoastLayer$y-0.09
colnames(CoastLayer) <- c("Coastdist", "longitude", "latitude")
CoastLayer$Coastdist <- as.numeric(CoastLayer$Coastdist)
#reducing No of observations to on-coast and less than a 100 km from coast
CoastLayer <- subset(CoastLayer, Coastdist <=0)
CoastLayer$Coastdist <- CoastLayer$Coastdist*-1
CoastLayer$Coastdist  <- ifelse(CoastLayer$Coastdist>= 120, 120, CoastLayer$Coastdist)

CoastMap <- BaseMap + geom_tile(aes(x = longitude, y = latitude, fill=Coastdist), data = CoastLayer, alpha=0.85) +
  scale_fill_gradient(low = "lightblue", high= "darkblue") +
  geom_point(aes(x=longitude, y=latitude-0.1, color="white"), data = ExA, size = 5 ) +
  geom_point(aes(x=longitude, y=latitude-0.1, color="red"), data = ExA, size = 4 ) +
  geom_point(aes(x=longitude, shape= inUC, color=inUC, y=latitude-0.1), data = ExA, size = 8 ) +
  geom_text(aes(x=longitude+0.3, y=latitude+0.19, fontface="bold", colour="firebrick", fontsize = 7), data=ExA, label=ExA$Date) +
  scale_shape_manual(values=c(10, 10, 13, 13)) +
  scale_colour_manual(values = c("white","white", "white","black", "firebrick", "firebrick")) +
  theme(legend.position="none")

plot(CoastMap)
rm(RASTERCoast, e)
unlink("Downloaded_Data/DICGSH1a.tif")


# Population Density
# Data sourced from http://neo.sci.gsfc.nasa.gov/servlet/RenderData?si=875430&cs=rgb&format=TIFF&width=3600&height=1800 and saved to Downloaded_Data
RASTERDens <- raster("Downloaded_Data/SEDAC_POP_2000-01-01_rgb_3600x1800.TIFF")
#crop Raster to Turkey maps boundaries
e <- extent(27.9, 34.9, 36.2, 41.7)
DensLayer <- crop(RASTERDens, e)
DensLayer <- as(DensLayer, "SpatialPixelsDataFrame")
DensLayer <- data.frame(DensLayer)
DensLayer$y <- DensLayer$y-0.09
colnames(DensLayer) <- c("Density", "longitude", "latitude")
DensLayer$Density <- as.numeric(DensLayer$Density)

# 255 is like NA, e.g. the sea
DensLayer <- subset(DensLayer, Density<255)
DensLayer$Density   <- ifelse(DensLayer$Density <= 100, 100, DensLayer$Density )


DensMap <- BaseMap + geom_tile(aes(x = longitude, y = latitude, fill=Density), data = DensLayer, alpha=0.75) +  
  scale_fill_gradient(low = "lightgreen", high= "firebrick") +
  geom_point(aes(x=longitude, y=latitude-0.1, color="white"), data = ExA, size = 5 ) +
  geom_point(aes(x=longitude, y=latitude-0.1, color="red"), data = ExA, size = 4 ) +
  geom_point(aes(x=longitude, shape= inUC, color=inUC, y=latitude-0.1), data = ExA, size = 8 ) +
  geom_text(aes(x=longitude+0.3, y=latitude+0.19, fontface="bold", colour="firebrick", fontsize = 7), data=ExA, label=ExA$Date) +
  scale_shape_manual(values=c(10, 10, 13, 13)) +
  scale_colour_manual(values = c("white","white", "white","black", "firebrick", "firebrick")) +
  theme(legend.position="none")
plot(DensMap)
  
 


rm(RASTERDens, e)
