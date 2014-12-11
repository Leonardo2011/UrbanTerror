# This script visualizes our Data Gathering, Merging and Manipulating Process with the example of Turkey
source("0 - Loading Packages.R")
PreGTD <- read.csv("TerrorData/pregtd.csv")

#find map of Turkey suitable for analysis. We identify Davulga as a community in mid-Turkey that enables us to find a map 
# wit appropriate zoom. The approach of bounding boxes does not work with googlemaps' raster approach

Turkey <- qmap("Davulga", zoom = 7, extent = "device", legend = "topleft")
# We did not find a solution to display a map based on bounding box coordinates - it is not compatible with googlemaps.
# However, googlemaps provides the information of any given box through the messages while searching in:
# http://maps.googleapis.com/maps/api/geocode/json?address=Davulga&sensor=false
# Box coordinates for our map:
# northeast: latitude = 38.984335, longitude = 31.3795199; southwest: latitude = 38.975781, longitude = 31.3690561

turkeymap <- subset(PreGTD, country_txt=="turkey" & !is.na(latitude) & iyear >= 1990)
sum.hum <- aggregate(turkeymap$HUMscale, by=list(turkeymap$WCUC.city.old), FUN=sum)
max.hum <- aggregate(turkeymap$HUMscale, by=list(turkeymap$WCUC.city.old), FUN=max)
sum.prop <- aggregate(turkeymap$PROPscale, by=list(turkeymap$WCUC.city.old), FUN=sum)
turkeymap  <- merge(turkeymap , sum.hum, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
turkeymap  <- merge(turkeymap , max.hum, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
turkeymap  <- merge(turkeymap , sum.prop, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
turkeymap["sum.hum"]<-turkeymap$x.x
turkeymap["max.hum"]<-turkeymap$x.y
turkeymap["sum.prop"]<-turkeymap$x
turkeymap$x <- NULL
turkeymap$x.y <- NULL
turkeymap$x.x <- NULL 

unzip("Downloaded_Data/Downloaded_Raster_Data.zip", exdir="Downloaded_Data")

# Stable light shining at night in mean reflection levels 1-63 from 1992 to 2010
# http://worldgrids.org/doku.php?id=wiki:layers

RASTERlight<- raster("Downloaded_Data/LNMDMS2a.tif")

Y <- rasterToPoints(RASTERlight)
YY <- data.frame(Y)
colnames(YY) <- c("lon", "lat", "light")
YYY <- subset(YY, lon >=27.9 & lon <= 34.9 & lat >= 36.2 & lat <=41.7)
YYY$light <- as.numeric(YYY$light)
YYY <- subset (YYY, light > 3)
YYY$light <- recode (YYY$light, "3:20 = 1; 21:30 = 2; 31:40 = 3; 41:63 = 4", as.numeric.result=TRUE)
LightMap <- Turkey +
          stat_density2d(aes(x = lon, y = lat, alpha =..level.., fill=..level..), data = YYY, geom = "polygon") +
          geom_point(aes(x = longitude, y = latitude, size=sum.hum, color=turkeymap$inUC), data = turkeymap, size=3) + 
          theme(legend.position="none") + 
          scale_fill_gradient(low = "black", high= "white")


unlink("Downloaded_Data/NLDI_2006_0p25_rev20111230.tif")
unlink("Downloaded_Data/GDP_grid_flt.tif")
unlink("Downloaded_Data/LNMDMS2a.tif")
unlink("Downloaded_Data/DICGSH1a.tif")
unlink("Downloaded_Data/G19ESA3a.tif")

