#Merge attempt of matching the GTD attacks with the corresponding WDI Urban Population data.


#We need to figure out an elegenat way to rename columns. As long as we can't fix this (plyr's rename does not seem to work), 
#we add a new column and delete the old one.


GTDWDI <-merge(GTD, WDIData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE, sort=TRUE)
