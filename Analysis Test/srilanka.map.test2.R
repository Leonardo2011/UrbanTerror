
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("foreign", "car", "RCurl", "ggplot2", "WDI", "httr", "iterators", "dplyr", "plyr", "stargazer",
              "XML", "maps", "ggmap", "Imap", "geosphere", "maptools", "rgeos", "foreach", "DataCombine", "mapproj", "rworldmap")
ipak(packages)
rm(packages)
rm(ipak)

unique(PreGTD$region_txt)



#subset the database
buthanmap <- subset(PreGTD, !is.na(latitude))
sum.hum <- aggregate(buthanmap$HUMscale, by=list(buthanmap$WCUC.city), FUN=sum)
max.hum <- aggregate(buthanmap$HUMscale, by=list(buthanmap$WCUC.city), FUN=max)
sum.prop <- aggregate(buthanmap$PROPscale, by=list(buthanmap$WCUC.city.old), FUN=sum)
buthanmap  <- merge(buthanmap , sum.hum, by.y=c("Group.1"), by.x=c("WCUC.city"), all.x=TRUE)
buthanmap  <- merge(buthanmap , max.hum, by.y=c("Group.1"), by.x=c("WCUC.city"), all.x=TRUE)
buthanmap  <- merge(buthanmap , sum.prop, by.y=c("Group.1"), by.x=c("WCUC.city"), all.x=TRUE)
buthanmap["sum.hum"]<-buthanmap$x.x
buthanmap$sum.hum <- ifelse(buthanmap$sum.hum >=5000, 5000, buthanmap$sum.hum)
buthanmap$sum.hum <- ifelse(buthanmap$sum.hum >=1500 & buthanmap$sum.hum<=4999, 1500, buthanmap$sum.hum)
buthanmap$sum.hum <- ifelse(buthanmap$sum.hum >=1000 & buthanmap$sum.hum<=1499, 1000, buthanmap$sum.hum)
buthanmap$sum.hum <- ifelse(buthanmap$sum.hum >=500 & buthanmap$sum.hum<=999, 500, buthanmap$sum.hum)
buthanmap$sum.hum <- ifelse(buthanmap$sum.hum >=100 & buthanmap$sum.hum<=499, 100, buthanmap$sum.hum)
buthanmap$sum.hum <- ifelse(buthanmap$sum.hum >=10 & buthanmap$sum.hum<=99, 50, buthanmap$sum.hum)
buthanmap$sum.hum <- ifelse(buthanmap$sum.hum <=10, 10, buthanmap$sum.hum)
buthanmap$sum.hum <- as.numeric(buthanmap$sum.hum)
buthanmap["max.hum"]<-buthanmap$x.y
buthanmap["sum.prop"]<-buthanmap$x
buthanmap["lon"]<-buthanmap$longitude
buthanmap["lat"]<-buthanmap$latitude
buthanmap$inUC<-as.factor(buthanmap$inUC)
buthanmap$x <- NULL
buthanmap$x.y <- NULL
buthanmap$x.x <- NULL 
rm(sum.hum, max.hum, sum.prop)
buthanmap <- buthanmap[!duplicated(buthanmap$merge),]

slmap <- qmap('el salvador', zoom = 6, legend = "topleft", legend="none")
FinalMap <- slmap + geom_point(aes(x=lon, y=lat, size=sum.hum, color=inUC), data = buthanmap) + scale_size_continuous(range=c(2, 5))
print(FinalMap)




#subset the database
colombomap <- subset(PreGTD, country_txt=="srilanka" & !is.na(latitude))
sum.hum <- aggregate(colombomap$HUMscale, by=list(colombomap$WCUC.city.old), FUN=sum)
max.hum <- aggregate(colombomap$HUMscale, by=list(colombomap$WCUC.city.old), FUN=max)
sum.prop <- aggregate(colombomap$PROPscale, by=list(colombomap$WCUC.city.old), FUN=sum)
colombomap  <- merge(colombomap , sum.hum, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
colombomap  <- merge(colombomap , max.hum, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
colombomap  <- merge(colombomap , sum.prop, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
colombomap["sum.hum"]<-colombomap$x.x
colombomap$sum.hum <- ifelse(colombomap$sum.hum >=1500, 1500, colombomap$sum.hum)
colombomap$sum.hum <- ifelse(colombomap$sum.hum >=1000 & colombomap$sum.hum<=1499, 1000, colombomap$sum.hum)
colombomap$sum.hum <- ifelse(colombomap$sum.hum >=500 & colombomap$sum.hum<=999, 500, colombomap$sum.hum)
colombomap$sum.hum <- ifelse(colombomap$sum.hum >=100 & colombomap$sum.hum<=499, 100, colombomap$sum.hum)
colombomap$sum.hum <- ifelse(colombomap$sum.hum >=10 & colombomap$sum.hum<=99, 50, colombomap$sum.hum)
colombomap$sum.hum <- ifelse(colombomap$sum.hum <=10, 10, colombomap$sum.hum)
colombomap$sum.hum <- as.numeric(colombomap$sum.hum)
colombomap["max.hum"]<-colombomap$x.y
colombomap["sum.prop"]<-colombomap$x
colombomap["lon"]<-colombomap$longitude
colombomap["lat"]<-colombomap$latitude
colombomap$inUC<-as.factor(colombomap$inUC)
colombomap$x <- NULL
colombomap$x.y <- NULL
colombomap$x.x <- NULL 

rm(sum.hum, max.hum, sum.prop)

colombomap <- colombomap[!duplicated(colombomap$merge),]

clmap <- qmap('Colombo', zoom = 6,  
              maptype = "satellite",
              legend = "topleft", legend="none")

FinalMapC <- clmap + geom_point(aes(x=lon, y=lat, size=sum.hum, color=inUC), data = colombomap) 
print(FinalMapC)

# + scale_size_continuous(range=c(3, 15)) + inset_raster(RASTERlight, -180, 180, -90, 90)