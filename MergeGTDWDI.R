#Merge attempt of matching the GTD attacks with the corresponding WDI Urban Population data.


#We need to figure out an elegenat way to rename columns. As long as we can't fix this (plyr's rename does not seem to work), 
#we add a new column and delete the old one.
WDIUrbanDat["country_txt"] <- WDIUrbanDat$country
WDIUrbanDat$country <- NULL
WDIUrbanDat["iyear"] <- WDIUrbanDat$year
WDIUrbanDat$year <- NULL

GTDWDI <-merge(GTD, WDIUrbanDat, by=c("country_txt", "iyear"), all.x = TRUE , sort=TRUE, suffixes=c(".G", ".W"))

