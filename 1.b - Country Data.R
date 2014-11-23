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
############# Country Level Data #############
##############################################


#############################################################################################################
###################################### 1. World Development Indicators ######################################
#############################################################################################################

###### Gathering Data  ######


#The World Bank offers large datasets on information about the distribution of population. Most importantly, these include urban 
#population. The datasets are available for download on the World Banks Website (http://data.worldbank.org/). Though downloadable,
#data there is not presented in a tidy way. Vincent Arel-Bundocks' WDI package (https://github.com/vincentarelbundock/WDI) offers
#fuctions to download data through the World Bank's API in a tidy way. If not done yet, either install the WDI package or 
#run Packages.R from our repository

#With WDISearch(), we can look for a list of indicators we are interested in containing the character string specified as an 
#argument. Regular Experessions can be used. We are interested in a subset of all indicators that contain [Uu]rban.
#We now let the WDI() function create our dataframe with all the urban indicators we are interested in. We also include the 
#extra argument, which adds information that is typically presented together with the urban indicators. Next we order the data
#first by country, then by year.

WDIData <- WDI(indicator=c('EN.URB.LCTY.UR.ZS',
                              'EN.URB.MCTY',
                              'EN.URB.MCTY.TL.ZS',
                              'SP.URB.GROW', 
                              'SP.URB.TOTL', 
                              'SP.POP.TOTL', 
                              'SP.URB.TOTL.IN.ZS',
                              'EN.POP.DNST',
                              'EN.RUR.DNST',
                              'SP.RUR.TOTL',
                              'SP.RUR.TOTL.ZG',
                              'SP.RUR.TOTL.ZS'),
                              country="all", start=1970, end=2013, extra=FALSE,
                    )
WDIData <- WDIData[order(WDIData$country, WDIData$year), ]

#We leave the World Bank codings of the indicators unchanged. Though they would be easier human-readable if we used their full
#names, the codes are easier to google. They directly link to exact the definitions used by the World Bank. The codes stand for:
#EN.URB.LCTY.UR.ZS = Population in the largest city (% of urban population)
#EN.URB.MCTY = Population in urban agglomerations of more than 1 million
#EN.URB.MCTY.TL.ZS = Population in urban agglomerations of more than 1 million (% of total population)
#SP.URB.GROW = Urban population growth (annual %)
#SP.URB.TOTL = Urban population
#SP.URB.TOTL.IN.ZS = Urban population (% of total
#EN.POP.DNST = Population density (people per sq. km of land area)
#EN.RUR.DNST = Rural population density (rural population per sq. km of arable land)
#SP.RUR.TOTL = Rural population                                                         
#SP.RUR.TOTL.ZG = Rural population growth (annual %)                                       
#SP.RUR.TOTL.ZS = Rural population (% of total population)
WDIData["EN.URB.LCTY.UR"] <- (WDIData$SP.POP.TOTL*WDIData$EN.URB.LCTY.UR.ZS/100)
#EN.URB.LCTY.UR = Population in the largest city


# finding 3  maxima for each country (total Pop, Pop in Urban Cemters, Pop in largest City)
Highs.URB.TOTL <- aggregate(as.numeric(WDIData$SP.URB.TOTL), list(country=WDIData$country),max)
colnames(Highs.URB.TOTL)[2] <- "MAX.URB.TOTL"
Highs.URB.MCTY <- aggregate(as.numeric(WDIData$EN.URB.MCTY), list(country=WDIData$country),max)
colnames(Highs.URB.MCTY)[2] <- "MAX.URB.MCTY"
Highs.URB.LCTY.UR <- aggregate(as.numeric(WDIData$EN.URB.LCTY.UR), list(country=WDIData$country),max)
colnames(Highs.URB.LCTY.UR )[2] <- "MAX.URB.LCTY.UR"

# bringing it back into the WDI Data
WDIData <- merge(WDIData, Highs.URB.TOTL, by=("country"))
WDIData <- merge(WDIData, Highs.URB.MCTY, by=("country"))
WDIData <- merge(WDIData, Highs.URB.LCTY.UR, by=("country"))
rm(Highs.URB.TOTL, Highs.URB.MCTY, Highs.URB.LCTY.UR)

#MAX.URB.TOTL = Urban population at its highestt for
#MAX.URB.MCTY = Population in urban agglomerations of more than 1 million at its highestt for
#MAX.LCTY.UR = Population in the largest city


###### Cleaning Data  ######

#Eliminating all Regions and other aggregation of countries.

WDIData <- subset(WDIData, 
                    WDIData$country != "Africa" &
                    WDIData$country != "Arab World" &
                    WDIData$country != "Andean Region" &
                    WDIData$country != "Caribbean small states" &         
                    WDIData$country != "Central Europe and the Baltics" &   
                    WDIData$country != "East Asia & Pacific (all income levels)" &   
                    WDIData$country != "East Asia & Pacific (developing only)" &   
                    WDIData$country != "East Asia and the Pacific (IFC classification)" &   
                    WDIData$country != "Euro area" &   
                    WDIData$country != "Europe & Central Asia (all income levels)" &   
                    WDIData$country != "Europe & Central Asia (developing only)" &   
                    WDIData$country != "Europe and Central Asia (IFC classification)" &   
                    WDIData$country != "European Union" &
                    WDIData$country != "Fragile and conflict affected situations" &
                    WDIData$country != "Heavily indebted poor countries (HIPC)" &
                    WDIData$country != "High income" &
                    WDIData$country != "High income: nonOECD" &
                    WDIData$country != "High income: OECD" &
                    WDIData$country != "Latin America & Caribbean (all income levels)" &
                    WDIData$country != "Latin America & Caribbean (developing only)" &
                    WDIData$country != "Latin America and the Caribbean" &
                    WDIData$country != "Latin America and the Caribbean (IFC classification)" &
                    WDIData$country != "Least developed countries: UN classification" &
                    WDIData$country != "Low & middle income" &
                    WDIData$country != "Low income" &
                    WDIData$country != "Lower middle income" &
                    WDIData$country != "Mexico and Central America" &
                    WDIData$country != "Middle East & North Africa (all income levels)" &
                    WDIData$country != "Middle East & North Africa (developing only)" &
                    WDIData$country != "Middle East and North Africa (IFC classification)" &
                    WDIData$country != "Middle income" &
                    WDIData$country != "North Africa" &
                    WDIData$country != "North America" &
                    WDIData$country != "Not classified" &
                    WDIData$country != "OECD members" &
                    WDIData$country != "Other small states" &
                    WDIData$country != "Pacific island small states" &
                    WDIData$country != "Small states" &
                    WDIData$country != "South Asia" &
                    WDIData$country != "South Asia (IFC classification)" &
                    WDIData$country != "Southern Cone Extended" &
                    WDIData$country != "Sub-Saharan Africa (all income levels)" &
                    WDIData$country != "Sub-Saharan Africa (developing only)" &
                    WDIData$country != "Sub-Saharan Africa (IFC classification)" &
                    WDIData$country != "Sub-Saharan Africa excluding South Africa" &
                    WDIData$country != "Sub-Saharan Africa excluding South Africa and Nigeria" &
                    WDIData$country != "Upper middle income" &
                    WDIData$country != "World"
)


#############################################################################################################
############################################## 2. War Data ##################################################
#############################################################################################################


# Data Wariable 1 out of 4:  Intra-State Wars from the COW Data Set Version v.4.1
# codebook: http://www.correlatesofwar.org/COW2%20Data/WarData_NEW/Intra-StateWars_Codebook.pdf

#download, define and subset
x <- getURL("www.correlatesofwar.org/COW2%20Data/WarData_NEW/Intra-StateWarData_v4.1.csv")
COWIS <- read.csv(text = x)
rm(x)
COWIS <- subset(COWIS, select=c(SideA, SideB, StartYear1, EndYear1))
COWIS$EndYear1 <- as.numeric(COWIS$EndYear1)
COWIS$StartYear1 <- as.numeric(COWIS$StartYear1)
COWIS$SideA <- as.character(COWIS$SideA)
COWIS$SideB <- as.character(COWIS$SideB)

# some cleaning, as we need the country level data on WDI Coding level to merge later
COWIS$SideA[COWIS$SideA == "Bosnia" | COWIS$SideA == "Bosnian Serbs"] <- "Bosnia and Herzegovina"
COWIS$SideA[COWIS$SideA == "Burma"] <- "Myanmar"
COWIS$SideA[COWIS$SideA == "Bosnian Serbs"] <- "Bosnia"
COWIS$SideA[COWIS$SideA == "Congo (Brazzaville)" | COWIS$SideA == "FDU (Cobra militia)"] <- "Congo, Rep."
COWIS$SideA[COWIS$SideA == "Democratic Republic of Congo"] <- "Congo, Dem. Rep."
COWIS$SideA[COWIS$SideA == "DRC"] <- "Congo, Dem. Rep."
COWIS$SideA[COWIS$SideA == "Guinea "] <- "Guinea"
COWIS$SideA[COWIS$SideA == "Iran"] <- "Iran, Islamic Rep."
COWIS$SideA[COWIS$SideA == "Laos"] <- "Lao PDR"
COWIS$SideA[COWIS$SideA == "Nepal "] <- "Nepal"
COWIS$SideA[COWIS$SideA == "Palestinians"] <- "West Bank and Gaza"
COWIS$SideA[COWIS$SideA == "Phillippines"] <- "Philippines"
COWIS$SideA[COWIS$SideA == "Rhodesia"] <- "Zimbabwe"
COWIS$SideA[COWIS$SideA == "Yugoslavia"| COWIS$SideA == "Yugoslavia/Serbia"] <- "Serbia"
COWIS$SideA[COWIS$SideA == "Russia" | COWIS$SideA == "Chechnya"] <- "Russian Federation"
COWIS$SideA[COWIS$SideA == "South Yemen"] <- "Yemen, Rep."
COWIS$SideA[COWIS$SideA == "Syria"] <- "Syrian Arab Republic"
COWIS$SideA[COWIS$SideA == "Tadzhikistan"] <- "Tajikistan"
COWIS$SideA[COWIS$SideA == "United States of America"] <- "United States"
COWIS$SideA[COWIS$SideA == "Yemen"] <- "Yemen, Rep."
COWIS$SideB[COWIS$SideB == "Bosnia" | COWIS$SideB == "Bosnian Serbs"] <- "Bosnia and Herzegovina"
COWIS$SideB[COWIS$SideB == "Burma"] <- "Myanmar"
COWIS$SideB[COWIS$SideB == "Congo (Brazzaville)" | COWIS$SideB == "FDU (Cobra militia)"] <- "Congo, Rep."
COWIS$SideB[COWIS$SideB == "Democratic Republic of Congo"] <- "Congo, Dem. Rep."
COWIS$SideB[COWIS$SideB == "DRC"] <- "Congo, Dem. Rep."
COWIS$SideB[COWIS$SideB == "Guinea "] <- "Guinea"
COWIS$SideB[COWIS$SideB == "Iran"] <- "Iran, Islamic Rep."
COWIS$SideB[COWIS$SideB == "Laos"] <- "Lao PDR"
COWIS$SideB[COWIS$SideB == "KLA"] <- "Kosovo"
COWIS$SideB[COWIS$SideB == "Nepal "] <- "Nepal"
COWIS$SideB[COWIS$SideB == "Palestinians"] <- "West Bank and Gaza"
COWIS$SideB[COWIS$SideB == "Phillippines"] <- "Philippines"
COWIS$SideB[COWIS$SideB == "Rhodesia"] <- "Zimbabwe"
COWIS$SideB[COWIS$SideB == "Russia" | COWIS$SideB == "Chechnya"] <- "Russian Federation"
COWIS$SideB[COWIS$SideB == "South Yemen"] <- "Yemen, Rep."
COWIS$SideB[COWIS$SideB == "Syria"] <- "Syrian Arab Republic"
COWIS$SideB[COWIS$SideB == "Tadzhikistan"] <- "Tajikistan"
COWIS$SideB[COWIS$SideB == "United States of America"] <- "United States"
COWIS$SideB[COWIS$SideB == "Yemen"] <- "Yemen, Rep."

# Bring this War variable merging ready on 0-1 per country and year.

COWIS$CountryA <- ifelse(COWIS$SideA %in% WDIData$country, COWIS$SideA, NA)
COWIS$CountryB <- ifelse(COWIS$SideB %in% WDIData$country, COWIS$SideB, NA)

COWIS.A <- COWIS[complete.cases(COWIS[,5]),]
COWIS.A <- subset(COWIS.A, select= c(StartYear1, EndYear1, CountryA), EndYear1>=1970, rownames=FALSE)
COWIS.B <- COWIS[complete.cases(COWIS[,6]),]
COWIS.B <- subset(COWIS.B, select= c(StartYear1, EndYear1, CountryB), EndYear1>=1970, rownames=FALSE)
COWIS.A <- rename(COWIS.A , c("CountryA"="Country"))
COWIS.B <- rename(COWIS.B , c("CountryB"="Country"))
COWIS1 <- merge (COWIS.A, COWIS.B, by=c("StartYear1", "EndYear1", "Country"), all=TRUE)
COWIS1 <- TimeFill(COWIS1, GroupVar = 'Country', StartVar = 'StartYear1', EndVar = 'EndYear1')
COWIS1 <- rename(COWIS1 , c("TimeFilled"="Intra.WAR")) 

#############################################################################################################
# Data Wariable 2 out of 4:  Inter-State Wars from the COW Data Set Version v.4.0
# codebook: http://www.correlatesofwar.org/COW2%20Data/WarData_NEW/Inter-StateWars_Codebook.pdf

#download, define and subset
x <- getURL("http://www.correlatesofwar.org/COW2%20Data/WarData_NEW/Inter-StateWarData_v4.0.csv")
COWIS2 <- read.csv(text = x)
rm(x)
COWIS2 <- subset(COWIS2, select=c(StateName, StartYear1, EndYear1), EndYear1>=1970, rownames=FALSE)
COWIS2$StateName <- as.character(COWIS2$StateName)
COWIS2$EndYear1 <- as.numeric(COWIS2$EndYear1)
COWIS2$StartYear1 <- as.numeric(COWIS2$StartYear1)


# some cleaning, as we need the country level data on WDI coding level to merge later
COWIS2$StateName[COWIS2$StateName == "Egypt"] <- "Egypt, Arab Rep."
COWIS2$StateName[COWIS2$StateName == "South Korea"] <- "Korea, Dem. Rep."
COWIS2$StateName[COWIS2$StateName == "South Vietnam"] <- "Vietnam"
COWIS2$StateName[COWIS2$StateName == "Bosnia" | COWIS2$StateName == "Bosnian Serbs"] <- "Bosnia and Herzegovina"
COWIS2$StateName[COWIS2$StateName == "Burma"] <- "Myanmar"
COWIS2$StateName[COWIS2$StateName == "Congo (Brazzaville)" | COWIS2$StateName == "FDU (Cobra militia)"] <- "Congo, Rep."
COWIS2$StateName[COWIS2$StateName == "DRC" | COWIS2$StateName == "Democratic Republic of Congo" | 
                   COWIS2$StateName == "Democratic Republic of the Congo"] <- "Congo, Dem. Rep."
COWIS2$StateName[COWIS2$StateName == "Iran"] <- "Iran, Islamic Rep."
COWIS2$StateName[COWIS2$StateName == "Laos"] <- "Lao PDR"
COWIS2$StateName[COWIS2$StateName == "Palestinians"] <- "West Bank and Gaza"
COWIS2$StateName[COWIS2$StateName == "Rhodesia"] <- "Zimbabwe"
COWIS2$StateName[COWIS2$StateName == "Yugoslavia"] <- "Serbia"
COWIS2$StateName[COWIS2$StateName == "Russia"] <- "Russian Federation"
COWIS2$StateName[COWIS2$StateName == "Syria"] <- "Syrian Arab Republic"
COWIS2$StateName[COWIS2$StateName == "Tadzhikistan"] <- "Tajikistan"
COWIS2$StateName[COWIS2$StateName == "United States of America"] <- "United States"
COWIS2$StateName[COWIS2$StateName == "Yemen" | COWIS2$StateName == "South Yemen"] <- "Yemen, Rep."

# Bring this War variable merging ready on 0-1 per country and year.
COWIS2$StateName <- ifelse(COWIS2$StateName %in% WDIData$country, COWIS2$StateName, NA)
COWIS2 <- COWIS2[complete.cases(COWIS2[,1:3]),]
COWIS2 <- TimeFill(COWIS2, GroupVar = 'StateName', StartVar = 'StartYear1', EndVar = 'EndYear1')
COWIS2 <- rename(COWIS2 , c("TimeFilled"="Inter.WAR")) 



#############################################################################################################
# Data Wariable 3 + 4 out of 4:  Extra-State Wars from the COW Data Set Version v.4.0
# codebook: http://www.correlatesofwar.org/COW2%20Data/WarData_NEW/Extra-StateWars_Codebook.pdf

#download, define and subset
x <- getURL("http://www.correlatesofwar.org/COW2%20Data/WarData_NEW/Extra-StateWarData_v4.0.csv")
COWIS3 <- read.csv(text = x)
rm(x)
COWIS3$EndYear1 <- recode(COWIS3$EndYear1, "-7=2014", as.numeric.result=TRUE)
COWIS3$EndYear1 <- as.numeric(COWIS3$EndYear1)
COWIS3$StartYear1 <- as.numeric(COWIS3$StartYear1)
COWIS3$SideA <- as.character(COWIS3$SideA)
COWIS3$SideB <- as.character(COWIS3$SideB)
COWIS3 <- subset(COWIS3, select=c(SideA, SideB, StartYear1, EndYear1), EndYear1>=1970, rownames=FALSE)

# some cleaning, as we need the country level data on WDI coding level to merge later
COWIS3$SideA[COWIS3$SideA == "Russia" | COWIS3$SideA == "USSR"] <- "Russian Federation"
COWIS3$SideB[COWIS3$SideB == "Palestinians"] <- "West Bank and Gaza"

# Bring this War variables merging ready on 0-1 per country and year.
COWIS3$CountryA <- ifelse(COWIS3$SideA %in% WDIData$country, COWIS3$SideA, NA)
COWIS3$CountryB <- ifelse(COWIS3$SideB %in% WDIData$country, COWIS3$SideB, NA)
COWIS3.A <- COWIS3[complete.cases(COWIS3[,5]),]
COWIS3.A <- subset(COWIS3.A, select= c(StartYear1, EndYear1, CountryA), EndYear1>=1970, rownames=FALSE)
COWIS3.B <- COWIS3[complete.cases(COWIS3[,6]),]
COWIS3.B <- subset(COWIS3.B, select= c(StartYear1, EndYear1, CountryB), EndYear1>=1970, rownames=FALSE)
COWIS3.A <- rename(COWIS3.A , c("CountryA"="Country"))
COWIS3.B <- rename(COWIS3.B , c("CountryB"="Country"))
COWIS3.A <- TimeFill(COWIS3.A, GroupVar = 'Country', StartVar = 'StartYear1', EndVar = 'EndYear1')
COWIS3.B <- TimeFill(COWIS3.B, GroupVar = 'Country', StartVar = 'StartYear1', EndVar = 'EndYear1')
COWIS3.A <- rename(COWIS3.A , c("TimeFilled"="Extra.WAR.In")) 
COWIS3.B <- rename(COWIS3.B , c("TimeFilled"="Extra.WAR.Out"))

# old code brought them together int one, but the 'receiver of an extra-terrirorrial war (Iraq)
# must be in anothe role as the 'donor' (USA, UK etc)
#COWIS3 <- merge (COWIS3.A, COWIS3.B, by=c("StartYear1", "EndYear1", "Country"), all=TRUE)


###### prepare War Variables for merging ######

COWIS1 <- rename(COWIS1 , c("Country"="country", "Time"="year"))
COWIS2 <- rename(COWIS2 , c("StateName"="country", "Time"="year"))
COWIS3.A <- rename(COWIS3.A , c("Country"="country", "Time"="year"))
COWIS3.B <- rename(COWIS3.B , c("Country"="country", "Time"="year"))




#############################################################################################################
#################################### War Variable Merging  with WDI #########################################
#############################################################################################################


# prepare WDI
WDIData$year <- as.numeric(WDIData$year)
WDIData$country <- as.character(WDIData$countr)

# rename, just to make sure
WDI_n_WAR <- WDIData

# merge Variable War variable 1, 2, 3, 4 

# Intra.WAR
WDI_n_WAR <- merge(WDI_n_WAR, COWIS1, by=c("country", "year"), all.x=TRUE)
WDI_n_WAR$Intra.WAR <- recode(WDI_n_WAR$Intra.WAR, "NA=0", as.numeric.result=TRUE)

# Inter.WAR
WDI_n_WAR <- merge(WDI_n_WAR, COWIS2, by=c("country", "year"), all.x=TRUE)
WDI_n_WAR$Inter.WAR <- recode(WDI_n_WAR$Inter.WAR, "NA=0", as.numeric.result=TRUE)

# Extra.WAR.In
WDI_n_WAR <- merge(WDI_n_WAR, COWIS3.A, by=c("country", "year"), all.x=TRUE)
WDI_n_WAR$Extra.WAR.In <- recode(WDI_n_WAR$Extra.WAR.In, "NA=0", as.numeric.result=TRUE)

# Extra.WAR.Out
WDI_n_WAR <- merge(WDI_n_WAR, COWIS3.B, by=c("country", "year"), all.x=TRUE)
WDI_n_WAR$Extra.WAR.Out <- recode(WDI_n_WAR$Extra.WAR.Out, "NA=0", as.numeric.result=TRUE)

#rm
rm(COWIS, COWIS.A, COWIS.B, COWIS3.A, COWIS3.B, COWIS1, COWIS2, COWIS3)




###### Rename, form and write CSV ######
CountryData <- WDI_n_WAR 

X <- CountryData$country
source('SmallScripts/CleanSpecialCharacters.R')
CountryData$country <- X
rm(X)

#create cache WDIData.csv (last: 15.11.2014)
write.csv(CountryData, "Cache/CountryData.csv")
