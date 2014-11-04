# MPP-E1180: Introduction to Collaborative Social Science Data Analysis
### Fall 2014
### Instructor: Christopher Gandrud

############################
####### URBAN TERROR #######
############################
###### Part 1: Data   ######
############################
#Lukas B Cameron R Sascha S#
############################
###### Cleaning Data  ######
############################


#Loading R packages using @stevenworthington's ipak.R gist from https://gist.github.com/stevenworthington/3178163.

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("foreign", "car", "RCurl", "ggplot2", "WDI", "httr", "dplyr", "XML", "maps")
ipak(packages)


########################################################################################
########################################################################################
############################   GATHERING  DATA    ######################################
########################################################################################
########################################################################################



############################################
###### The Global Terrorism Database  ######
############################################


############################################
#Load the Global Terrorism Database (GTD). It is open souce and can be downloaded after registration at 
# http://www.start.umd.edu/gtd/contact/

rawGTD <- read.csv("Terror Data/globalterrorismdb_0814dist.csv", header=TRUE)


#The (GTD) contains over a 120k observations on more than 120 variables. We don't need them all. 
#We therefore filter the database to make it fit our needs, erasing over a 100 variables. 
#We only want to look at successfull terror attacks and include basic data on time, location and target.
GTD <- subset(rawGTD, select = c(eventid, iyear, imonth, iday, country, region, city, attacktype1, targtype1, targsubtype1,
                                weaptype1, weapsubtype1, propextent, nkill, nwound), 
                                iyear >= 1970 & success == 1, na.strings = c("", " "))


############################################
#We introduce our first scale: "Targets Urbanity Potential Scale (TUPscale)"

GTD["TUPscale"] <- GTD$targsubtype1
GTD$TUPscale <- recode(GTD$TUPscale, "40:42 = 9; 9 = 0; 27:35 = 0; 37:39 = 0; 65 = 0; 72 = 0; 1 = 2; 4:5 = 2; 10 = 2;
                       12 = 2; 53:56 = 2; 58:59 = 2; 61:62 = 2; 82 = 2; 95:96 = 2;6 = 9; 13 = 9; 104:108 = 9; 
                       51:52 = 9; 57 = 9; 60 = 9; 63:64 = 9; 73 = 9; 80:81 = 9; 88:92 = 9; 98 = 9; 2 = 7; 3 = 7; 
                       7:8 = 7; 44 = 7; 48:50 = 7; 67:71 = 7; 74:79 = 7; 83:87 = 7; 97 = 7; 99 = 7; 14:26 = 9; 
                       100:103 = 9; 111 = 9; 109 = 9; 110 = 9; 36 = 9; 43 = 9; 45:47 = 9; 66 = 9; 93:94 = 9; 
                       11 = 9", as.numeric.result=TRUE)


# 0= Rural & Military; 2= Government & Police; 3= Potentilly Urban Workplace; #7= Potentilly Urban Infrastructure; 
# 9= Potentilly Expression of Urban Core Life


############################################
# We introduce our second scale: "Extent of Property Damage (PROPscale)" and write it back into the GTD

GTD["PROPscale"] <- GTD$propextent
GTD$PROPscale <- as.numeric(GTD$PROPscale)

#Bring the values to the $ values coded in the originally coded categories. 
GTD$PROPscale <- recode(GTD$PROPscale, "1=1000000000; 2=1000000; 3=1000; 4=0; NA=NA")


############################################
# We introduce our third scale: "Extent of Human Damage (HUMscale)" which adds wounded and killed /and write it back into the GTD

GTD["HUMscale"] <- GTD$nkill+GTD$nwound
GTD$HUMscale <- as.numeric(GTD$HUMscale)


############################################
# We introduce our second scale: "Extent of Property Damage (PROPscale)"

GTD["PROPscale"] <- GTD$propextent
GTD$PROPscale <- as.numeric(GTD$PROPscale)

#Bring down to values between 0 and 1 that reflect our appreciation for the vast gaps between the originally coded categories. 
GTD$PROPscale <- recode(GTD$PROPscale, "1=1; 2=0.002; 3=0.001; 4=0; NA=0")

### We introduce our second scale: "Extent of Human Damage (HUMscale)" 
GTD["HUMscale"] <- GTD$nkill+GTD$nwound
GTD$HUMscale <- as.numeric(GTD$HUMscale)

#Bring down to values between 0 and 1 and normalize to the same sum as GTD$PROPscale
GTD$HUMscale <- (GTD$HUMscale*sum(GTD$PROPscale, na.rm = TRUE)/sum(GTD$HUMscale, na.rm = TRUE))



###########################################
############## BIG CITY DATA ##############
###########################################

# here: http://download.maxmind.com/download/worldcities/worldcitiespop.txt.gz and transformed into CSV
worldcities2013 <- read.csv("City Data/worldcitiespop.csv")

### cities with a known population with more than 100.000 inhabitants
worldcities2013_over_100k <- subset(worldcities2013, select = c(Country, City, AccentCity, Region, Latitude, Longitude, Population), Population > 100000)


### list the world capital cities
data(world.cities)
world.cities$name <- tolower(world.cities$name)
world.cities2009 <- world.cities[order(-world.cities$pop, na.last=TRUE) , ]
rm(world.cities)
capitals <- subset(world.cities2009, select = c(name, country.etc, pop), capital == 1)



#########################################
########## URBAN CENTERS DATA ###########
#########################################

# Scrap Wiki on urban Centers
URL <- 'http://en.wikipedia.org/w/index.php?title=List_of_urban_areas_by_population&section=2'


#clean up the Urban Centers name in order to align with City Names
table <- readHTMLTable(URL)
UrbanCenters <- table [[2]] 
UrbanCenters$City <- gsub("\\[.+?\\]","", UrbanCenters$City)
UrbanCenters$City <- gsub("\\(.+?\\)","", UrbanCenters$City)
UrbanCenters$City <- gsub("[[:digit:]]", "", UrbanCenters$City)
UrbanCenters$City <- gsub("[[:punct:]]", "", UrbanCenters$City)
UrbanCenters$City <- tolower(UrbanCenters$City)



############################################
############# World Bank Data  #############
############################################


############################################
#Load the World Bank Data on the nominal Urban Population 
WB_Urban_Pop = WDI(indicator='SP.URB.TOTL', country='all', start=1970, end=2013)



########################################################################################
########################################################################################
############################   MERGING  DATA    ########################################
########################################################################################
########################################################################################


#### try to merge, not leading anywhere but if city names in UrbanCenters are cleaned, it could lead somewhere
#UrbanCenters["AccentCity"] <- UrbanCenters$City
#try <- merge(Cities_over_50k, UrbanCenters, by=c("AccentCity"))
