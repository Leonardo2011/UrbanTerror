#Merge attempt of matching the GTD attacks with the corresponding WDI Urban Population data.


#We need to figure out an elegenat way to rename columns. As long as we can't fix this (plyr's rename does not seem to work), 
#we add a new column and delete the old one.
WDIData["country_txt"] <- WDIData$country
WDIData$country <- NULL
WDIData["iyear"] <- WDIData$year
WDIData$year <- NULL

GTDWDI <-merge(GTD, WDIData, by=c("country_txt", "iyear"), all.x = TRUE , sort=TRUE, suffixes=c(".G", ".W"))

