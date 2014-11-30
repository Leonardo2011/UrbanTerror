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
######## Global Terrorism Database ##########
##############################################


###### Global Terrorism Database ######


###### Gathering Data  ######

#Load the Global Terrorism Database (GTD). It is open souce and can be downloaded after registration at 
# http://www.start.umd.edu/gtd/contact/

rawGTD <- read.csv("TerrorData/globalterrorismdb_0814dist.csv", header=TRUE)


#The (GTD) contains over a 120k observations on more than 120 variables. Many seem irrelevant to our analysis. 
#We subset the database, selecting also only successful terror attacks.
#Documentation on the variables we select can be found on: http://www.start.umd.edu/gtd/downloads/Codebook.pdf

GTD <- subset(rawGTD, select = c(eventid, iyear, imonth, iday, country, country_txt, region, provstate, region_txt, city, attacktype1, targtype1, targsubtype1,
                                 weaptype1, weapsubtype1, propextent, nkill, nwound, latitude, longitude), 
              iyear >= 1970 & success == 1, na.strings = c("", " "))
rm(rawGTD)


###### Cleaning Data  ######

# clean lat long data a little bit
GTD$latitude[GTD$latitude=="" | GTD$latitude==" "]  <- NA
GTD$longitude[GTD$longitude=="" | GTD$longitude==" "]  <- NA



#Eliminating inconsistencies in the GTD's countr_txt column and bring to WDI standard.
GTD$country_txt <- as.character(GTD$country_txt)

# 1. Combining formerly seperated countries

GTD$country_txt[GTD$country_txt == "East Germany (GDR)"] <- "Germany"
GTD$country_txt[GTD$country_txt == "West Germany (FRG)"] <- "Germany"
GTD$country_txt[GTD$country_txt == "North Yemen"] <- "Yemen"
GTD$country_txt[GTD$country_txt == "South Yemen"] <- "Yemen"
GTD$country_txt[GTD$country_txt == "Falkland Islands" ] <- "United Kingdom"
GTD$country_txt[GTD$country_txt == "New Hebrides" ] <- "Vanuatu"
GTD$country_txt[GTD$country_txt == "South Vietnam" ] <- "Vietnam"

# 2. Splitting up formerly united countries 

#Since we have WDI data going back 1970 on a per country scale also for countries
#that were not independent in 1970 (like the Soviet Union countries) we can just treat the cities the attack took place as 
#being part of the country they are a part of now.

# 2.1. The Soviet Union
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Yerevan" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Meghri" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Aygepar" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Erubuni" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Charektar" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Kirovakan" ] <- "Armenia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Yevlakh" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Aterk" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Gyandzha" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Tatli" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Tallinn" ] <- "Estonia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Ikla" ] <- "Estonia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Tbilisi" ] <- "Georgia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Osh" ] <- "Kyrgyzstan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Riga" ] <- "Latvia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Ainazi" ] <- "Latvia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Lavoriskes" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Birzai" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Vilnius" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Salociai" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Medininkai" ] <- "Lithuania"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Moscow" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Buynaksk" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Kara-Su" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Novgorod" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Vasyurinskaya" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Ostankinskiy" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Grebenskaya" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Temirgoye" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Irkutsk" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Sverdlovsk" ] <- "Russia"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Leningrad" ] <- "Russia"

# 2.2. Czechoslovakia
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Prague" ] <- "Czech Republic"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Usti Nad Labem" ] <- "Czech Republic"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Halickuv Brod" ] <- "Czech Republic"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Hradec Kralove" ] <- "Czech Republic"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" & GTD$city =="Bratislava" ] <- "Slovak Republic"

# 2.3. Serbia-Montenegro
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Bujanovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Zarkovo" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Presevo" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Belgrade" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Dobrosin" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Subotica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Velki Trnovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Novi Pazar" ] <- "Serbia"

# 2.4. Yugoslavia
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bogicevica" ] <- "Albania"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Banjica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Belgrade" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Blace" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Borovica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bujanovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bujic" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Cacak" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Cerevajka" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dolovo" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Konculj" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Lisani" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Loznica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Medvedja" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Muhovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Nis" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Novi Sad" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Oraovica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Popuke" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Pozarevac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Subotica" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Surcin" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Valjevo" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Velika Reka" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Veliki Trnovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Velki Trnovac" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Vranje" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Zemun" ] <- "Serbia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Celopek" ] <- "Macedonia, FYR"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Prilep" ] <- "Macedonia, FYR"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Skopje" ] <- "Macedonia, FYR"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Berane" ] <- "Montenegro"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Budva" ] <- "Montenegro"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Cetinje" ] <- "Montenegro"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Podgorica" ] <- "Montenegro"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Ljubljana" ] <- "Slovenia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Stari" ] <- "Slovenia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Stari Trg" ] <- "Slovenia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Benkovac" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Borovac" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dolac"] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dubrovnik" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dvor na Uni" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kijevo" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Knin" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Lipik" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Osijek" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Pakrac" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Petrinja" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Polaca" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Pribudic" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Sotin" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Titova Korenica" ] <- "Croatia"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Batlava" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Besinje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bradas" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Careva Cesma" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Decani" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Djakovica" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Donje Ljupce" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Donje Prekaze" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Donji Crnobreg" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Dulje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Glogovac" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Gornja Klina" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Grabanic‰" ] <- "Kosovo"  ######## This may be an encoding problem
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Grabc" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Istinic" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Klincina" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kline" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Komoran" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kosovo Polje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kosovska Mitrovica" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Krpimej" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kruska" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Ljubenic" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Magur‰" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Malisevo" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Opterusa" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Orlat" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Peje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Podujeva" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Podujev‰" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Prekale" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Pristina" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Prizren" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Runik" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Srbica" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Stimlje" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Streoc" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Surkis" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Suva Reka" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Titova Mitrovica" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Tusuz" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Urosevac" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Voksh" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Vucitrn" ] <- "Kosovo"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Banja Luka" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bijelina" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Bosanski Brod" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Kalesija" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Mostar" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Odzak" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Yugoslavia" & GTD$city =="Sarajevo" ] <- "Bosnia and Herzegovina"

# 3. Problematic recodings (e.g. contested regions, typos etc.)

#The following all seem to belong to the Nagorno-Karabakh region, contested by Armenia and Azerbaijan, but currently governed by 
#Azerbaijan, though populated mostly by Armenians.
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Nagorno-Karabakh (Autonomous Oblast)" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Askeran" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Askeran (District)" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Khojali" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Khodzhaly" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Baganis-Airum" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="baganis-airum" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Airum Pirili" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Chaily" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Drmbon" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Stepanakert" ] <- "Azerbaijan"
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Kazakh" ] <- "Azerbaijan"

#This attack was coded as having happened in Serbia-Montenegro, but according to lang-log data, the place the attack occurred is
# in Estonia
GTD$country_txt[GTD$country_txt == "Serbia-Montenegro" & GTD$city =="Levosje" ] <- "Estonia"

#These are two attacks in the early 1990s that seem to have targeted the Ararat Region outside of any city.
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Unknown" & GTD$provstate == "Armenia" ] <- "Armenia"

#It is on Crimea. We make a judgement call here and go for the country currently in charge.
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Feodocia" ] <- "Russia"

# This is Abchasia
GTD$country_txt[GTD$country_txt == "Soviet Union" & GTD$city =="Sukhumi" ] <- "Georgia"

# This is one attack happening in the Gulf of Aden, off the coast of Yemen
GTD$country_txt[GTD$country_txt == "International" ] <- "Yemen"

# 4. Renaming Countries to make them match the WDIData

GTD$country_txt[GTD$country_txt == "Bahamas" ] <- "Bahamas, The"
GTD$country_txt[GTD$country_txt == "Bosnia-Herzegovina" ] <- "Bosnia and Herzegovina"
GTD$country_txt[GTD$country_txt == "Central African Republic" ] <- "Central African Republic"
GTD$country_txt[GTD$country_txt == "Congo (Brazzaville)" ] <- "Congo, Dem. Rep."
GTD$country_txt[GTD$country_txt == "Congo (Kinshasa)" ] <- "Congo, Rep."
GTD$country_txt[GTD$country_txt == "Corsica" ] <- "France"
GTD$country_txt[GTD$country_txt == "Czechoslovakia" ] <- ""
GTD$country_txt[GTD$country_txt == "Egypt" ] <- "Egypt, Arab Rep."
GTD$country_txt[GTD$country_txt == "French Guiana" ] <- "France"
GTD$country_txt[GTD$country_txt == "Gambia" ] <- "Gambia, The"
GTD$country_txt[GTD$country_txt == "Great Britain" ] <- "United Kingdom"
GTD$country_txt[GTD$country_txt == "Guadeloupe" ] <- "France"
GTD$country_txt[GTD$country_txt == "Hong Kong" ] <- "Hong Kong SAR, China"
GTD$country_txt[GTD$country_txt == "Iran" ] <- "Iran, Islamic Rep."
GTD$country_txt[GTD$country_txt == "Ivory Coast" ] <- "Cote d'Ivoire"
GTD$country_txt[GTD$country_txt == "Kyrgyzstan" ] <- "Kyrgyz Republic"
GTD$country_txt[GTD$country_txt == "Laos" ] <- "Lao PDR"
GTD$country_txt[GTD$country_txt == "Macau" ] <- "Macao SAR, China"
GTD$country_txt[GTD$country_txt == "Macedonia" ] <- "Macedonia, FYR"
GTD$country_txt[GTD$country_txt == "Martinique" ] <- ""
GTD$country_txt[GTD$country_txt == "North Korea" ] <- "Korea, Dem. Rep."
GTD$country_txt[GTD$country_txt == "Northern Ireland" ] <- "United Kingdom"
GTD$country_txt[GTD$country_txt == "Rhodesia" ] <- "Zimbabwe"
GTD$country_txt[GTD$country_txt == "Russia" ] <- "Russian Federation"
GTD$country_txt[GTD$country_txt == "South Korea" ] <- "Korea, Rep."
GTD$country_txt[GTD$country_txt == "Syria" ] <- "Syrian Arab Republic"
GTD$country_txt[GTD$country_txt == "Taiwan" ] <- "China"
GTD$country_txt[GTD$country_txt == "Venezuela" ] <- "Venezuela, RB"
GTD$country_txt[GTD$country_txt == "Wallis and Futuna" ] <- "France"
GTD$country_txt[GTD$country_txt == "West Bank and Gaza Strip" ] <- "West Bank and Gaza"
GTD$country_txt[GTD$country_txt == "Western Sahara" ] <- "Spain"
GTD$country_txt[GTD$country_txt == "Yemen" ] <- "Yemen, Rep."



# Run a small script to eliminate special characters and standardize coding representations.
X <- GTD$country_txt
source('SmallScripts/CleanSpecialCharacters.R')
GTD$country_txt <- X
rm(X)

X <- GTD$city
source('SmallScripts/CityCleaning.R')
source('SmallScripts/CleanSpecialCharacters.R')
GTD$city <- X
rm(X)


###### Manipulate Data  ######

#We add several columns to the GTD database that will be used in our analysis.
#The GTD contains a categorical target type variable with about 90 categories. 
#We combine them to 5 different categories in our "Targets Urbanity Potential Scale (TUPscale)"

# 0= Rural & Military; 2= Government & Police; 3= Potentilly Urban Workplace; #7= Potentilly Urban Infrastructure; 
# 9= Potentilly Expression of Urban Core Life

#These are the original GTD categories and how we recoded them:
#TUP, GTD_CodeNum, GTD_Code_txt
#0,40,Clinics
#0,41,Personnel
#0,42,Aircraft (not at an airport)
#1,27,Military Barracks/Base/Headquarters/Check Post
#1,28,Military Recruiting Station/Academy
#1,29,Military Units/Patrols/Convoys
#1,30,Navy
#1,31,Air
#1,32,Coast Guard
#1,33,Army
#1,34,Soldiers/Troops/Officers/Forces
#1,35,Military Transportation/Vehicles (excluding specific mentions of convoys)
#1,37,North Atlantic Treaty Organization (NATO) Related
#1,38,Marine
#1,39,Paramilitary
#1,65,Refugee Camps
#1,72,Farmers
#1,9,Farm/Ranch
#2,109,Party Officials/Candidates/Other Personnel
#2,110,Party Offices/Facilities
#2,14,Judges/Attorneys/Courts
#2,15,Politicians/Political Parties/Political Movements/Political Party Meetings/Rallies
#2,16,Royalty
#2,17,Head of State
#2,18,Government employees (excluding police/military)
#2,19,Election Related
#2,20,Intelligence
#2,21,Government Buildings/Facilities/Offices
#2,22,Police Buildings (Headquarters/Stations/School)
#2,23,Police Patrol (including vehicles and convoys)
#2,24,Police Checkpoint
#2,25,Police Security Forces/Officers
#2,26,Prisons/Jails
#2,43,Airline Officer/Personnel
#2,45,Diplomats/Families/Individuals (outside of embassy)
#2,46,Embassies/Consulates
#2,47,International Organizations (Aid Agencies/Compounds/Peacekeepers)
#2,66,Demilitarized Zones (including Green Zone)
#2,93,Terrorist Organization
#2,94,Non_State Militia
#3,1,Gas/oil
#3,10,Mining
#3,12,Construction
#3,4,Multinational Corporations
#3,5,Industrial/Textiles/Factories
#3,53,Newspaper Journalists/Staff/Facilities
#3,54,Radio Journalists/Staff/Facilities
#3,55,Television Journalists/Staff/Facilities
#3,56,Other (including online news agencies)
#3,58,Commercial Maritime
#3,59,Oil Tankers
#3,61,Domestic NGO
#3,62,International NGO
#3,82,Labor Union Related
#3,95,Tourism Travel Agency
#3,96,Tour Bus/Van/Vehicle
#4,104,Highway/Road/Toll/Traffic Signal
#4,105,Taxi/Rickshaw
#4,106,Gas
#4,107,Electricity
#4,108,Oil
#4,13,Private Security Companies/Firms
#4,51,Food Supply
#4,52,Water Supply
#4,57,Civilian Maritime
#4,6,Medical/Pharmaceutical
#4,60,Ports
#4,63,Ambulances
#4,64,Fire Fighters/Trucks
#4,73,Vehicles/Transportation
#4,80,Memorials/Cemeteries/Monuments
#4,81,Museums/Cultural Centers/Cultural Houses
#4,88,Radio
#4,89,Television
#4,90,Telephone/Telegraph
#4,91,Internet Infrastructure
#4,92,Multiple Telecommunication Targets
#4,98,Other Facilities
#5,100,Train/Train Tracks/ Trolley
#5,101,Bus Station/Stop
#5,102,Subway
#5,103,Bridge/Car Tunnel
#5,11,Entertainment/Cultural/Stadiums/Casinos
#5,111,Rallies
#5,2,Restaurant/Bar/Caf
#5,3,Bank/Commerce
#5,44,Airport
#5,48,Teachers/Professors/Instructors
#5,49,Schools/Universities/Educational Buildings
#5,50,Other Personnel
#5,67,Unnamed Civilians/Unspecified
#5,68,Named civilians
#5,69,Religion Identified
#5,7,Retail/Grocery/Bakery (including cell phone shops and generic shops)
#5,70,Students
#5,71,Race/Ethnicity Identified
#5,75,Village/Cities/Towns/Suburb (large areas)
#5,76,Houses/Apartments/Residence
#5,77,Laborers (General)/Specific Jobs
#5,78,Processions/Gatherings (Funerals/Weddings/Birthdays/Religious)
#5,79,Public Areas (e.g., Public garden, parking lot, garage, beach, public buildings,
#5,8,Hotel/Resort
#5,83,Protestors
#5,84,Political Party Members/Rallies
#5,85,Religious Figures
#5,86,Places of Worship
#5,87,Affiliated Institutions
#5,99,Bus (excluding tourist)

GTD["TUPscale"] <- GTD$targsubtype1
GTD$TUPscale <- recode(GTD$TUPscale, "40:42 = 9; 9 = 0; 27:35 = 0; 37:39 = 0; 65 = 0; 72 = 0; 1 = 2; 4:5 = 2; 10 = 2;
                       12 = 2; 53:56 = 2; 58:59 = 2; 61:62 = 2; 82 = 2; 95:96 = 2;6 = 9; 13 = 9; 104:108 = 9; 
                       51:52 = 9; 57 = 9; 60 = 9; 63:64 = 9; 73 = 9; 80:81 = 9; 88:92 = 9; 98 = 9; 2 = 7; 3 = 7; 
                       7:8 = 7; 44 = 7; 48:50 = 7; 67:71 = 7; 74:79 = 7; 83:87 = 7; 97 = 7; 99 = 7; 14:26 = 9; 
                       100:103 = 9; 111 = 9; 109 = 9; 110 = 9; 36 = 9; 43 = 9; 45:47 = 9; 66 = 9; 93:94 = 9; 
                       11 = 9", as.numeric.result=TRUE)

#We introduce our second scale: "Extent of Property Damage (PROPscale)" and write it back into the GTD
GTD["PROPscale"] <- GTD$propextent
GTD$PROPscale <- as.numeric(GTD$PROPscale)

#Bring the values to the $ values coded in the originally coded categories. 
GTD$PROPscale <- recode(GTD$PROPscale, "1=1000000000; 2=1000000; 3=1000; 4=0; NA=0")

# We introduce our third scale: "Extent of Human Damage (HUMscale)" which adds wounded and killed /and write it back into the GTD
GTD$nkill <- recode(GTD$nkill, "NA=0")
GTD$nwound <- recode(GTD$nwound, "NA=0")
GTD["HUMscale"] <- GTD$nkill+GTD$nwound
GTD$HUMscale <- as.numeric(GTD$HUMscale)

#create cache GTD.csv (last: 15.11.2014)
write.csv(GTD, "Cache/GTD.csv")
