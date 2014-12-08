ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("foreign", "car", "RCurl", "ggplot2", "WDI", "httr", "iterators", "dplyr", "plyr", "stargazer",
              "XML", "maps", "ggmap", "Imap", "geosphere", "maptools", "rgeos", "foreach", "DataCombine")
ipak(packages)
rm(packages)
rm(ipak)

#download a limited GTD we created for this assignment (5MB instead of 100MB file)
PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/TerrorData/Pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(PreGTD_in_Memory,'Pre.GTD.csv')
rm(PreGTD_in_Memory)
PreGTD <- read.csv("Pre.GTD.csv", header=TRUE)

WC.UC.dist_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/Cache/WC.UC.dist.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(WC.UC.dist_in_Memory,'WC.UC.dist.csv')
rm(WC.UC.dist_in_Memory)
WC.UC.dist <- read.csv("WC.UC.dist.csv", header=TRUE)


#find map of sri lanka
qmap('Sri Lanka', zoom = 7)

#subset the database
srilankamap <- subset(PreGTD, country_txt=="srilanka" & !is.na(latitude))
#subset the database
srilankamap <- subset(PreGTD, country_txt=="srilanka" & !is.na(latitude))
sum.hum <- aggregate(srilankamap$HUMscale, by=list(srilankamap$WCUC.city.old), FUN=sum)
max.hum <- aggregate(srilankamap$HUMscale, by=list(srilankamap$WCUC.city.old), FUN=max)
sum.prop <- aggregate(srilankamap$PROPscale, by=list(srilankamap$WCUC.city.old), FUN=sum)
srilankamap  <- merge(srilankamap , sum.hum, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
srilankamap  <- merge(srilankamap , max.hum, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
srilankamap  <- merge(srilankamap , sum.prop, by.y=c("Group.1"), by.x=c("WCUC.city.old"), all.x=TRUE)
srilankamap["sum.hum"]<-srilankamap$x.x
srilankamap["max.hum"]<-srilankamap$x.y
srilankamap["sum.prop"]<-srilankamap$x
srilankamap$x <- NULL
srilankamap$x.y <- NULL
srilankamap$x.x <- NULL 

rm(sum.hum, max.hum, sum.prop)



slmap <- qmap('Sri Lanka', zoom = 7,  
                   source = "stamen", maptype = "toner",
                   legend = "none")

# Add points
FinalMap <- slmap + geom_point(aes(x = longitude, y = latitude, size=sum.hum, color=merge), data = srilankamap)

print(FinalMap)



#try some overlays#
srilankamap <- srilankamap[!duplicated(srilankamap$merge),]

slmap + stat_density2d(aes(x = longitude, y = latitude, fill = (sum.hum), alpha = 1),
    size = 1, bins = 3, data = srilankamap,
    geom = "polygon")

overlay <- stat_density2d(
  aes(x = longitude, y = latitude, fill = hum.sum, alpha = 3/4),
  bins = 4, geom = "polygon",
  data = srilankamap)

HoustonMap + overlay + inset(
  grob = ggplotGrob(ggplot() + overlay + theme_inset()),
  xmin = -95.35836, xmax = Inf, ymin = -Inf, ymax = 29.75062)







##Extra###
srilankamap <- srilankamap[!duplicated(srilankamap$merge),]
# only violent crimes
violent_crimes <- subset(crime,
                         offense != "auto theft" & offense != "theft" &
                           offense != "burglary")

# order violent crimes
violent_crimes$offense <- factor(violent_crimes$offense,
                                 levels = c("robbery", "aggravated assault", "rape", "murder"))
# Set up base map
HoustonMap <- qmap("houston", zoom = 14,  
                   source = "stamen", maptype = "toner",
                   legend = "topleft")

# Add points
FinalMap <- HoustonMap +
  geom_point(aes(x = lon, y = lat, colour = offense,
                 size = offense),
             data = violent_crimes) +
  guides(size = guide_legend(title = 'Offense'),
         colour = guide_legend(title = 'Offense'))
