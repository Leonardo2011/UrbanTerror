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
library(foreign) 
library(car)
library(ggplot2)

setwd("C:/Users/Lokus/Dropbox/Master Thesis/GitHub-Repo/UrbanTerror")

#Load the Global Terrorism Database
rawGTD <- read.csv("globalterrorismdb_0814dist.csv", header=TRUE)

#The Global Terror Database (GTD) we are using for our analysis contains over a 120k observations on more than 120 variables. 
#The complete database contains redundant or dispensable data because we do not need it for our analysis.
#We therefore filter the database to make it fit our needs. We erase over a 100 variables. 
#We only want to look at successfull terror attacks and the ones that happened after 1989.

GTD <- subset(rawGTD, select = c(eventid, iyear, imonth, iday, country, region, attacktype1, targtype1, targsubtype1,
                                weaptype1, weapsubtype1, propextent, nkill, nwound), 
                                iyear >= 1989 & success == 1, na.strings = c("", " "))

### We introduce our first scale: "Targets Urbanity Potential Scale (TUPscale)" 
GTD["TUPscale"] <- GTD$targsubtype1
GTD$TUPscale <- recode(GTD$TUPscale, "40:42 = 3; 9 = 1; 27:35 = 1; 37:39 = 1; 65 = 1; 72 = 1; 1 = 2; 4:5 = 2; 10 = 2; 
                                      12 = 2; 53:56 = 2; 58:59 = 2; 61:62 = 2; 82 = 2; 95:96 = 2;6 = 3; 13 = 3;
                                      104:108 = 3; 51:52 = 3; 57 = 3; 60 = 3; 63:64 = 3; 73 = 3; 80:81 = 3; 88:92 = 3;
                                      98 = 3; 2 = 4; 3 = 4; 7:8 = 4; 44 = 4;  48:50 = 4; 67:71 = 4; 74:79 = 4; 83:87 = 4;
                                      97 = 4; 99 = 4; 14:26 = 5; 100:103 = 5; 111 = 5; 109 = 5; 110 = 5; 36 = 5; 43 = 5; 
                                      45:47 = 5; 66 = 5; 93:94 = 5")
# 1= Rural & Military; 2= Government & Police; 3= Potentilly Urban Workplace; 
#4= Potentilly Urban Infrastructure; 5= Potentilly Urban Core Life 

GTD$TUPscale <- as.numeric(GTD$TUPscale)
#Bring TUPscale down to values between 0 and 1 that reflect our appreciation for the targets potential urbanity.
GTD$TUPscale <- recode(GTD$TUPscale, "1=0; 2=0.1; 3=0.3; 4=0.6; 5=1")

### We introduce our second scale: "Extend of Property Damage (DAMscale)"
GTD["PROPscale"] <- GTD$propextent
GTD$PROPscale <- as.numeric(GTD$PROPscale)
#Bring down to values between 0 and 1 that reflect our appreciation for the vast gaps between the originally coded categories. 
GTD$PROPscale <- recode(GTD$PROPscale, "1=1; 2=0.002; 3=0.001; 4=0; NA=0")

### We introduce our second scale: "Extend of Human Damage (HUMscale)" 
GTD["HUMscale"] <- GTD$nkill+GTD$nwound
GTD$HUMscale <- as.numeric(GTD$HUMscale)
#Bring down to values between 0 and 1 and normalize to the same sum as GTD$PROPscale
GTD$HUMscale <- (GTD$HUMscale*sum(GTD$PROPscale, na.rm = TRUE)/sum(GTD$HUMscale, na.rm = TRUE))


#### prepare a data frame for plotting ###

DAMAGE <- GTD$HUMscale+GTD$PROPscale
DAMAGE_0_up_to_1 <- HUM/max(HUM, na.rm = TRUE)
TUP_weight_as_DAMAGE_0_up_to_1 <-GTD$TUPscale*sum(DAMAGE_0_up_to_1, na.rm = TRUE)/sum(GTD$TUPscale, na.rm = TRUE)

#now we have the damage scale and the targeting scale with the same weight, a mixed factor is now possible
TUPxDAMAGE <-TUP_weight_as_DAMAGE_0_up_to_1*DAMAGE_0_up_to_1
TUPandDAMAGE <-TUP_weight_as_DAMAGE_0_up_to_1+DAMAGE_0_up_to_1

# make dataframe for plotting
Plotframe1 <- data.frame(year=GTD$iyear, tup= TUP_weight_as_DAMAGE_0_up_to_1, dam= DAMAGE_0_up_to_1, 
                          tupxdamage= TUPxDAMAGE, tupanddamage= TUPandDAMAGE, region= GTD$region)


##!!!!!!!##########plots dont produce trendline########!!!!!!!!##
# plot for the addition of the two scales, damage and targets urbanity potential over time
#Plotframe1$year <- as.numeric(Plotframe1$year)
#Plotframe1$tupanddamage <- as.numeric(Plotframe1$tupanddamage)
#qplot(x=year, y=tupanddamage, data=Plotframe1, log="y", geom=c("point", "smooth"), method="lm")
#ggplot(data=Plotframe1,aes(x=year,y=tupanddamage) +geom_point() + stat_smooth(method="lm"))

