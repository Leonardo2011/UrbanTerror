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

#Load basic R packages
install.packages("car")
library(foreign) 
library(car)

#Load the Global Terrorism Database

rawGTD <- read.csv("globalterrorismdb_0814dist.csv", header=TRUE)

#The Global Terror Database (GTD) we are using for our analysis contains over a 120k observations on more than 120 variables. 
#The complete database contains redundant or dispensable data because we do not need it for our analysis.
#We therefore filter the database to make it fit our needs. We erase over a 100 variables. 
#We only want to look at successfull terror attacks and the ones that happened after 1989.

GTD <- subset(rawGTD, select = c(eventid, iyear, imonth, iday, country, region, attacktype1, targtype1, targsubtype1, weaptype1, weapsubtype1, propextent), iyear >= 1990 & success == 1, na.strings = c("", " "))

#We introduce our first scale: "Targets Urbanity Potential Scale (TUPscale)" 

GTD["TUPscale"] <- GTD$targsubtype1

GTD$TUPscale <- recode(GTD$TUPscale, "40:42 = 0; 9 = 1; 27:35 = 1; 37:39 = 1; 65 = 1; 72 = 1; 1 = 2; 4:5 = 2; 10 = 2; 12 = 2; 53:56 = 2; 58:59 = 2; 61:62 = 2; 82 = 2; 95:96 = 2;6 = 3; 13 = 3; 104:108 = 3; 51:52 = 3; 57 = 3; 60 = 3; 63:64 = 3; 73 = 3; 80:81 = 3; 88:92 = 3; 98 = 3; 2 = 4; 3 = 4; 7:8 = 4; 44 = 4;  48:50 = 4; 67:71 = 4; 74:79 = 4; 83:87 = 4; 97 = 4; 99 = 4; 14:26 = 5; 100:103 = 5; 111 = 5; 109 = 5; 110 = 5; 36 = 5; 43 = 5; 45:47 = 5; 66 = 5; 93:94 = 5")
