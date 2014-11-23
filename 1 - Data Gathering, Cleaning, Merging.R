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
Cities <- WC.UC.dist$name
Countries <- WC.UC.dist$country.etc
WC.UC.dist["merge"] <- paste(Countries, Cities, sep="")
Testframe <- GTDWDI
Testframe["merge"] <-data.frame(paste(GTDWDIcountry, GTDWDIcity, sep=""))
PreGTD <- merge(Testframe, WC.UC.dist, by=c("merge"), all.x=TRUE)
PreGTD  <- PreGTD [order(-PreGTD$eventid, na.last=TRUE) , ]

PreGTD <- subset(PreGTD, select=c(eventid, merge, iyear, city, pop, capital, region_txt, largestC, Closest.Urban.Center, 
                                  largest.UC, coastalMC, WC.UC.dist.km, part.of.urban.center, in.urban.centers.environment, attacktype1, 
                                  targtype1, targsubtype1, weaptype1, weapsubtype1, TUPscale, PROPscale, HUMscale, SP.POP.TOTL,
                                  EN.URB.LCTY.UR, MAX.URB.LCTY.UR, EN.URB.MCTY, MAX.URB.MCTY, SP.URB.TOTL, MAX.URB.TOTL,
                                  EN.POP.DNST, Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR))


PreGTD$part.of.urban.center[is.na(PreGTD$part.of.urban.center)] <- FALSE
PreGTD$in.urban.centers.environment[is.na(PreGTD$in.urban.centers.environment)] <- FALSE
PreGTD$in.urban.centers.environment <- recode(PreGTD$in.urban.centers.environment, "TRUE=1")
PreGTD$part.of.urban.center <- recode(PreGTD$part.of.urban.center, "TRUE=1")


PreGTD$capital[is.na(PreGTD$capital)] <- 0 
PreGTD$largestC[is.na(PreGTD$largestC)] <- 0 
PreGTD$largest.UC[is.na(PreGTD$largest.UC)] <- 0 
PreGTD$coastalMC[is.na(PreGTD$coastalMC)] <- 0 
PreGTD$pop[is.na(PreGTD$pop)] <- 0 


write.csv(PreGTD, file="TerrorData/Pregtd.csv")
rm(Testframe, GTDWDIcity, GTDWDIcountry, Cities, GTD, Countries, GTDWDI)



