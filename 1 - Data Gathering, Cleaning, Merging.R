# MPP-E1180: Introduction to Collaborative Social Science Data Analysis
### Fall 2014
### Instructor: Christopher Gandrud

##############################################
############## URBAN TERROR ##################
##############################################
###############  Part 1: DATA   ##############
##############################################
##########Lukas B Cameron R Sascha S##########
##############################################
####### Gathering, Cleaning, Merging #########
##############################################

# In this script we combine all our previously established databases into one, bringing together 
# terror data, country level data and city level data into one database ready for analysis.



#############################################################################################################
################### Loading Datasets previously gathered, cleaned and partially merged ######################
#############################################################################################################

# Global Terrorism Database (GTD) with some new scales and categories we introduced to the GTD for our purposes
if(file.exists("Cache/GTD.csv")) {GTD <- read.csv("Cache/GTD.csv")} else{source("1.a - Global Terrorism Database.R")}


# Country level data from the World Bank Development Indicators (WDI) and the The Correlates of War (COW) project data on wars.
if(file.exists("Cache/CountryData.csv")){CountryData <- read.csv("Cache/CountryData.csv")} else{source("1.b - Country Data.R")}


# City level data from a number of sources, including web scraping
if(file.exists("Cache/WC.UC.dist.csv")) {WC.UC.dist <- read.csv("Cache/WC.UC.dist.csv")} else{source("1.c - City Data.R")}



#############################################################################################################
############################# Merge the Data Sets into a "PreGTD"###########################################
#############################################################################################################


###### Merge GTD & Country Level Data ######

GTDWDI <- merge(GTD, CountryData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE, sort=TRUE)

###### Merge Combined set with City Data ######

GTDWDIcity <- GTDWDI$city
GTDWDIcountry <- GTDWDI$country_txt
Cities <- WC.UC.dist$old.name
Countries <- WC.UC.dist$country.etc
WC.UC.dist["merge"] <- paste(Countries, Cities, sep="")
WC.UC.dist <- WC.UC.dist[order(WC.UC.dist$merge, WC.UC.dist$capital, -WC.UC.dist$pop),]
WC.UC.dist <- WC.UC.dist[!duplicated(WC.UC.dist$merge), ]
Testframe <- GTDWDI
Testframe["merge"] <-data.frame(paste(GTDWDIcountry, GTDWDIcity, sep=""))
PreGTD <- merge(Testframe, WC.UC.dist, by=c("merge"), all.x=TRUE)
PreGTD  <- PreGTD [order(-PreGTD$HUMscale, na.last=TRUE) , ]

source('SmallScripts/dynamic_n_relative_CitySize.R')

# limit and order the new PreGTD
PreGTD <- subset(PreGTD, select=c(eventid, merge, iyear, imonth, iday, city, old.name, pop, Rel.CS, EN.URB.LCTY.UR,  capital, largestC, part.of.urban.center,
                                  Closest.Urban.Center,largest.UC, coastalMC, WC.UC.dist.km, attacktype1,targtype1, targsubtype1, weaptype1, weapsubtype1,
                                  TUPscale, PROPscale, HUMscale, Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR, old.pop, pop.today))




# write a csv, just to be sure
write.csv(PreGTD, file="TerrorData/Pregtd.csv")
rm(Testframe, GTDWDIcity, GTDWDIcountry, Cities, GTD, Countries, GTDWDI)




