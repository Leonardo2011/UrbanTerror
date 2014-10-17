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

Load basic R packages
library(plyr)           
library(ggplot2)
library(RColorBrewer)
library(foreign) 

#Load the Global Terrorism Database

rawGTD <- read.csv("globalterrorismdb_0814dist.csv", header=TRUE)

#The Global Terror Database (GTD) we are using for our analysis contains over a 120k observations on more than 120 variables. 
#The complete database contains redundant or dispensable data because we do not need it for our analysis.
#We therefore filter the database to make it fit our needs. We erase over a 100 variables. 
#We only want to look at successfull terror attacks and the ones that happened after 1989.

GTD <- subset(rawGTD, select = c(eventid, iyear, imonth, iday, country, region, attacktype1, targtype1, targsubtype1, weaptype1, weapsubtype1, propextent), iyear >= 1990 & success == 1, na.strings = c("", " "))

#We introduce our first scale: "Targets Urbanity Potential Scale (TUPscale)" 

GTD["TUPscale"] <- NA
