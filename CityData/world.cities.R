########################################
############# URBAN TERROR #############
########################################
############  Part I: Data   ###########
########################################
#######Lukas B Cameron R Sascha S#######
########################################


#############  City Data  ##############

#The purpose of this script is to gather, clean, and merge two different datasets on cities of the world with similar information.
#We merge them to increase the total number of cities that could match the GTD.

###### Gathering Data  ######

###World cities dataset 1/2 (worldcities2013)###

# it stems from a private company, Maxmind Inc.
#http://download.maxmind.com/download/worldcities/worldcitiespop.txt.gz and transformed into CSV, since it is not available
#in .csv format right away.
worldcities2013 <- read.csv("CityData/worldcitiespop.csv")


###World cities dataset 2/2 (world.cities from the 'maps' package)###

#The dataset is called world.cities. Since it is from 2009, we call it worldcities2009.
data(world.cities)
worldcities2009 <- world.cities
rm(world.cities)


###### Cleaning Data  ######


#Our standard for city names is lowercase letters with no special characters from the worldcities2009 dataset 
#for easier merging purposes, and lowercase country names (no special charackters) from the world.cities2013 dataset.

###World cities dataset 1/2 (worldcities2013)###

#The dataset lacks some information.

#introduce Tehran as it was missing in the original dataset
worldcities2013 <- rbind(worldcities2013, data.frame(X=0,Country="ir", City="tehran", AccentCity="Tehran", 
                                                     Region= 1, Latitude=35.67, Longitude=51.43,Population=7160094))

#introduce Akkaraipattu as it was missing in the original dataset
worldcities2013 <- rbind(worldcities2013, data.frame(X=0,Country="lk", City="Akkaraipattu", AccentCity="Akkaraipattu", 
                                                     Region= 31, Latitude=7.227862, Longitude=81.850551,Population=35000))


#The 2013 dataset uses Iso2c acronyms for country identification, which we recode to full country names.
X <- worldcities2013$Country
X<-gsub(" ","",X, ignore.case=TRUE)
X<-gsub("^AD$","Andorra",X, ignore.case=TRUE)
X<-gsub("^AE$","UnitedArabEmirates",X, ignore.case=TRUE)
X<-gsub("^AF$","Afghanistan",X, ignore.case=TRUE)
X<-gsub("^AG$$","AntiguaandBarbuda",X, ignore.case=TRUE)
X<-gsub("^AI$","Anguilla",X, ignore.case=TRUE)
X<-gsub("^AL$","Albania",X, ignore.case=TRUE)
X<-gsub("^AM$","Armenia",X, ignore.case=TRUE)
X<-gsub("^AN$","NetherlandsAntilles",X, ignore.case=TRUE)
X<-gsub("^AO$","Angola",X, ignore.case=TRUE)
X<-gsub("^AQ$","Antarctica",X, ignore.case=TRUE)
X<-gsub("^AR$","Argentina",X, ignore.case=TRUE)
X<-gsub("^AS$","AmericanSamoa",X, ignore.case=TRUE)
X<-gsub("^AT$","Austria",X, ignore.case=TRUE)
X<-gsub("^AU$","Australia",X, ignore.case=TRUE)
X<-gsub("^AW$","Aruba",X, ignore.case=TRUE)
X<-gsub("^AX$","ÅlandIslands",X, ignore.case=TRUE)
X<-gsub("^AZ$","Azerbaijan",X, ignore.case=TRUE)
X<-gsub("^BA$","BosniaandHerzegovina",X, ignore.case=TRUE)
X<-gsub("^BB$","Barbados",X, ignore.case=TRUE)
X<-gsub("^BD$","Bangladesh",X, ignore.case=TRUE)
X<-gsub("^BE$","Belgium",X, ignore.case=TRUE)
X<-gsub("^BF$","BurkinaFaso",X, ignore.case=TRUE)
X<-gsub("^BG$","Bulgaria",X, ignore.case=TRUE)
X<-gsub("^BH$","Bahrain",X, ignore.case=TRUE)
X<-gsub("^BI$","Burundi",X, ignore.case=TRUE)
X<-gsub("^BJ$","Benin",X, ignore.case=TRUE)
X<-gsub("^BL$","Saint-Barthelemy",X, ignore.case=TRUE)
X<-gsub("^BM$","Bermuda",X, ignore.case=TRUE)
X<-gsub("^BN$","Brunei",X, ignore.case=TRUE)
X<-gsub("^BO$","Bolivia",X, ignore.case=TRUE)
X<-gsub("^BQ$","Bonaire,SintEustatiusandSaba",X, ignore.case=TRUE)
X<-gsub("^BR$","Brazil",X, ignore.case=TRUE)
X<-gsub("^BS$","Bahamas",X, ignore.case=TRUE)
X<-gsub("^BT$","Bhutan",X, ignore.case=TRUE)
X<-gsub("^BV$","BouvetIsland",X, ignore.case=TRUE)
X<-gsub("^BW$","Botswana",X, ignore.case=TRUE)
X<-gsub("^BY$","Belarus",X, ignore.case=TRUE)
X<-gsub("^BZ$","Belize",X, ignore.case=TRUE)
X<-gsub("^CA$","Canada",X, ignore.case=TRUE)
X<-gsub("^CD$","CongoDemocraticRepublic",X, ignore.case=TRUE)
X<-gsub("^CF$","CentralAfricanRepublic",X, ignore.case=TRUE)
X<-gsub("^CG$","Congo",X, ignore.case=TRUE)
X<-gsub("^CH$","Switzerland",X, ignore.case=TRUE)
X<-gsub("^CI$","IvoryCoast",X, ignore.case=TRUE)
X<-gsub("^CK$","CookIslands",X, ignore.case=TRUE)
X<-gsub("^CL$","Chile",X, ignore.case=TRUE)
X<-gsub("^CM$","Cameroon",X, ignore.case=TRUE)
X<-gsub("^CN$","China",X, ignore.case=TRUE)
X<-gsub("^CO$","Colombia",X, ignore.case=TRUE)
X<-gsub("^CR$","CostaRica",X, ignore.case=TRUE)
X<-gsub("^CU$","Cuba",X, ignore.case=TRUE)
X<-gsub("^CV$","CapeVerde",X, ignore.case=TRUE)
X<-gsub("^CW$","Curaçao",X, ignore.case=TRUE)
X<-gsub("^CX$","ChristmasIsland",X, ignore.case=TRUE)
X<-gsub("^CY$","Cyprus",X, ignore.case=TRUE)
X<-gsub("^CZ$","CzechRepublic",X, ignore.case=TRUE)
X<-gsub("^DE$","Germany",X, ignore.case=TRUE)
X<-gsub("^DJ$","Djibouti",X, ignore.case=TRUE)
X<-gsub("^DK$","Denmark",X, ignore.case=TRUE)
X<-gsub("^DM$","Dominica",X, ignore.case=TRUE)
X<-gsub("^DO$","DominicanRepublic",X, ignore.case=TRUE)
X<-gsub("^DZ$","Algeria",X, ignore.case=TRUE)
X<-gsub("^EC$","Ecuador",X, ignore.case=TRUE)
X<-gsub("^EE$","Estonia",X, ignore.case=TRUE)
X<-gsub("^EG$","Egypt",X, ignore.case=TRUE)
X<-gsub("^EH$","WesternSahara",X, ignore.case=TRUE)
X<-gsub("^ER$","Eritrea",X, ignore.case=TRUE)
X<-gsub("^ES$","Spain",X, ignore.case=TRUE)
X<-gsub("^ET$","Ethiopia",X, ignore.case=TRUE)
X<-gsub("^FI$","Finland",X, ignore.case=TRUE)
X<-gsub("^FJ$","Fiji",X, ignore.case=TRUE)
X<-gsub("^FK$","FalklandIslands",X, ignore.case=TRUE)
X<-gsub("^FM$","Micronesia",X, ignore.case=TRUE)
X<-gsub("^FO$","FaroeIslands",X, ignore.case=TRUE)
X<-gsub("^FR$","France",X, ignore.case=TRUE)
X<-gsub("^GA$","Gabon",X, ignore.case=TRUE)
X<-gsub("^GB$","UnitedKingdom",X, ignore.case=TRUE)
X<-gsub("^GD$","Grenada",X, ignore.case=TRUE)
X<-gsub("^GE$","Georgia",X, ignore.case=TRUE)
X<-gsub("^GF$","FrenchGuiana",X, ignore.case=TRUE)
X<-gsub("^GG$","GuernseyandAlderney",X, ignore.case=TRUE)
X<-gsub("^GH$","Ghana",X, ignore.case=TRUE)
X<-gsub("^GI$","Gibraltar",X, ignore.case=TRUE)
X<-gsub("^GL$","Greenland",X, ignore.case=TRUE)
X<-gsub("^GM$","Gambia",X, ignore.case=TRUE)
X<-gsub("^GN$","Guinea",X, ignore.case=TRUE)
X<-gsub("^GP$","Guadeloupe",X, ignore.case=TRUE)
X<-gsub("^GQ$","EquatorialGuinea",X, ignore.case=TRUE)
X<-gsub("^GR$","Greece",X, ignore.case=TRUE)
X<-gsub("^GS$","SouthGeorgiaandtheSouthSandwichIslands",X, ignore.case=TRUE)
X<-gsub("^GT$","Guatemala",X, ignore.case=TRUE)
X<-gsub("^GU$","Guam",X, ignore.case=TRUE)
X<-gsub("^GW$","Guinea-Bissau",X, ignore.case=TRUE)
X<-gsub("^GY$","Guyana",X, ignore.case=TRUE)
X<-gsub("^HK$","HongKong",X, ignore.case=TRUE)
X<-gsub("^HM$","HeardIslandandMcDonaldIslands",X, ignore.case=TRUE)
X<-gsub("^HN$","Honduras",X, ignore.case=TRUE)
X<-gsub("^HR$","Croatia",X, ignore.case=TRUE)
X<-gsub("^HT$","Haiti",X, ignore.case=TRUE)
X<-gsub("^HU$","Hungary",X, ignore.case=TRUE)
X<-gsub("^ID$","Indonesia",X, ignore.case=TRUE)
X<-gsub("^IE$","Ireland",X, ignore.case=TRUE)
X<-gsub("^IL$","Israel",X, ignore.case=TRUE)
X<-gsub("^IM$","IsleofMan",X, ignore.case=TRUE)
X<-gsub("^IN$","India",X, ignore.case=TRUE)
X<-gsub("^IO$","BritishIndianOceanTerritory",X, ignore.case=TRUE)
X<-gsub("^IQ$","Iraq",X, ignore.case=TRUE)
X<-gsub("^IR$","Iran",X, ignore.case=TRUE)
X<-gsub("^IS$","Iceland",X, ignore.case=TRUE)
X<-gsub("^IT$","Italy",X, ignore.case=TRUE)
X<-gsub("^JE$","Jersey",X, ignore.case=TRUE)
X<-gsub("^JM$","Jamaica",X, ignore.case=TRUE)
X<-gsub("^JO$","Jordan",X, ignore.case=TRUE)
X<-gsub("^JP$","Japan",X, ignore.case=TRUE)
X<-gsub("^KE$","Kenya",X, ignore.case=TRUE)
X<-gsub("^KG$","Kyrgyzstan",X, ignore.case=TRUE)
X<-gsub("^KH$","Cambodia",X, ignore.case=TRUE)
X<-gsub("^KI$","Kiribati",X, ignore.case=TRUE)
X<-gsub("^KM$","Comoros",X, ignore.case=TRUE)
X<-gsub("^KN$","SaintKittsandNevis",X, ignore.case=TRUE)
X<-gsub("^KP$","Korea,DemocraticPeople'sRepublicof",X, ignore.case=TRUE)
X<-gsub("^KR$","Korea,Republicof",X, ignore.case=TRUE)
X<-gsub("^KW$","Kuwait",X, ignore.case=TRUE)
X<-gsub("^KY$","CaymanIslands",X, ignore.case=TRUE)
X<-gsub("^KZ$","Kazakhstan",X, ignore.case=TRUE)
X<-gsub("^LA$","Laos",X, ignore.case=TRUE)
X<-gsub("^LB$","Lebanon",X, ignore.case=TRUE)
X<-gsub("^LC$","SaintLucia",X, ignore.case=TRUE)
X<-gsub("^LI$","Liechtenstein",X, ignore.case=TRUE)
X<-gsub("^LK$","SriLanka",X, ignore.case=TRUE)
X<-gsub("^LR$","Liberia",X, ignore.case=TRUE)
X<-gsub("^LS$","Lesotho",X, ignore.case=TRUE)
X<-gsub("^LT$","Lithuania",X, ignore.case=TRUE)
X<-gsub("^LU$","LuXembourg",X, ignore.case=TRUE)
X<-gsub("^LV$","Latvia",X, ignore.case=TRUE)
X<-gsub("^LY$","Libya",X, ignore.case=TRUE)
X<-gsub("^MA$","Morocco",X, ignore.case=TRUE)
X<-gsub("^MC$","Monaco",X, ignore.case=TRUE)
X<-gsub("^MD$","Moldova",X, ignore.case=TRUE)
X<-gsub("^ME$","Montenegro",X, ignore.case=TRUE)
X<-gsub("^MF$","Saint-Martin",X, ignore.case=TRUE)
X<-gsub("^MG$","Madagascar",X, ignore.case=TRUE)
X<-gsub("^MH$","MarshallIslands",X, ignore.case=TRUE)
X<-gsub("^MK$","Macedonia",X, ignore.case=TRUE)
X<-gsub("^ML$","Mali",X, ignore.case=TRUE)
X<-gsub("^MM$","Myanmar",X, ignore.case=TRUE)
X<-gsub("^MN$","Mongolia",X, ignore.case=TRUE)
X<-gsub("^MO$","Macao",X, ignore.case=TRUE)
X<-gsub("^MP$","NorthernMarianaIslands",X, ignore.case=TRUE)
X<-gsub("^MQ$","Martinique",X, ignore.case=TRUE)
X<-gsub("^MR$","Mauritania",X, ignore.case=TRUE)
X<-gsub("^MS$","Montserrat",X, ignore.case=TRUE)
X<-gsub("^MT$","Malta",X, ignore.case=TRUE)
X<-gsub("^MU$","Mauritius",X, ignore.case=TRUE)
X<-gsub("^MV$","Maldives",X, ignore.case=TRUE)
X<-gsub("^MW$","Malawi",X, ignore.case=TRUE)
X<-gsub("^MX$","MeXico",X, ignore.case=TRUE)
X<-gsub("^MY$","Malaysia",X, ignore.case=TRUE)
X<-gsub("^MZ$","Mozambique",X, ignore.case=TRUE)
X<-gsub("^NA$","Namibia",X, ignore.case=TRUE)
X<-gsub("^NC$","NewCaledonia",X, ignore.case=TRUE)
X<-gsub("^NE$","Niger",X, ignore.case=TRUE)
X<-gsub("^NF$","NorfolkIsland",X, ignore.case=TRUE)
X<-gsub("^NG$","Nigeria",X, ignore.case=TRUE)
X<-gsub("^NI$","Nicaragua",X, ignore.case=TRUE)
X<-gsub("^NL$","Netherlands",X, ignore.case=TRUE)
X<-gsub("^NO$","Norway",X, ignore.case=TRUE)
X<-gsub("^NP$","Nepal",X, ignore.case=TRUE)
X<-gsub("^NR$","Nauru",X, ignore.case=TRUE)
X<-gsub("^NU$","Niue",X, ignore.case=TRUE)
X<-gsub("^NZ$","NewZealand",X, ignore.case=TRUE)
X<-gsub("^OM$","Oman",X, ignore.case=TRUE)
X<-gsub("^PA$","Panama",X, ignore.case=TRUE)
X<-gsub("^PE$","Peru",X, ignore.case=TRUE)
X<-gsub("^PF$","FrenchPolynesia",X, ignore.case=TRUE)
X<-gsub("^PG$","PapuaNewGuinea",X, ignore.case=TRUE)
X<-gsub("^PH$","Philippines",X, ignore.case=TRUE)
X<-gsub("^PK$","Pakistan",X, ignore.case=TRUE)
X<-gsub("^PL$","Poland",X, ignore.case=TRUE)
X<-gsub("^PM$","SaintPierreandMiquelon",X, ignore.case=TRUE)
X<-gsub("^PN$","Pitcairn",X, ignore.case=TRUE)
X<-gsub("^PR$","PuertoRico",X, ignore.case=TRUE)
X<-gsub("^PS$","Palestine",X, ignore.case=TRUE)
X<-gsub("^PT$","Portugal",X, ignore.case=TRUE)
X<-gsub("^PW$","Palau",X, ignore.case=TRUE)
X<-gsub("^PY$","Paraguay",X, ignore.case=TRUE)
X<-gsub("^QA$","Qatar",X, ignore.case=TRUE)
X<-gsub("^RE$","Reunion",X, ignore.case=TRUE)
X<-gsub("^RO$","Romania",X, ignore.case=TRUE)
X<-gsub("^RS$","SerbiaandMontenegro",X, ignore.case=TRUE)
X<-gsub("^RU$","RussianFederation",X, ignore.case=TRUE)
X<-gsub("^RW$","Rwanda",X, ignore.case=TRUE)
X<-gsub("^SA$","SaudiArabia",X, ignore.case=TRUE)
X<-gsub("^SB$","SolomonIslands",X, ignore.case=TRUE)
X<-gsub("^SC$","Seychelles",X, ignore.case=TRUE)
X<-gsub("^SD$","Sudan",X, ignore.case=TRUE)
X<-gsub("^SE$","Sweden",X, ignore.case=TRUE)
X<-gsub("^SG$","Singapore",X, ignore.case=TRUE)
X<-gsub("^SH$","SaintHelena",X, ignore.case=TRUE)
X<-gsub("^SI$","Slovenia",X, ignore.case=TRUE)
X<-gsub("^SJ$","SvalbardandJanMayen",X, ignore.case=TRUE)
X<-gsub("^SK$","Slovakia",X, ignore.case=TRUE)
X<-gsub("^SL$","SierraLeone",X, ignore.case=TRUE)
X<-gsub("^SM$","SanMarino",X, ignore.case=TRUE)
X<-gsub("^SN$","Senegal",X, ignore.case=TRUE)
X<-gsub("^SO$","Somalia",X, ignore.case=TRUE)
X<-gsub("^SR$","Suriname",X, ignore.case=TRUE)
X<-gsub("^SS$","SouthSudan",X, ignore.case=TRUE)
X<-gsub("^ST$","SaoTomeandPrincipe",X, ignore.case=TRUE)
X<-gsub("^SV$","ElSalvador",X, ignore.case=TRUE)
X<-gsub("^SX$","SintMaarten(Dutchpart)",X, ignore.case=TRUE)
X<-gsub("^SY$","Syria",X, ignore.case=TRUE)
X<-gsub("^SZ$","Swaziland",X, ignore.case=TRUE)
X<-gsub("^TC$","TurksandCaicos",X, ignore.case=TRUE)
X<-gsub("^TD$","Chad",X, ignore.case=TRUE)
X<-gsub("^TF$","FrenchSouthernTerritories",X, ignore.case=TRUE)
X<-gsub("^TG$","Togo",X, ignore.case=TRUE)
X<-gsub("^TH$","Thailand",X, ignore.case=TRUE)
X<-gsub("^TJ$","Tajikistan",X, ignore.case=TRUE)
X<-gsub("^TK$","Tokelau",X, ignore.case=TRUE)
X<-gsub("^TL$","Timor-Leste",X, ignore.case=TRUE)
X<-gsub("^TM$","Turkmenistan",X, ignore.case=TRUE)
X<-gsub("^TN$","Tunisia",X, ignore.case=TRUE)
X<-gsub("^TO$","Tonga",X, ignore.case=TRUE)
X<-gsub("^TR$","Turkey",X, ignore.case=TRUE)
X<-gsub("^TT$","TrinidadandTobago",X, ignore.case=TRUE)
X<-gsub("^TV$","Tuvalu",X, ignore.case=TRUE)
X<-gsub("^TW$","Taiwan",X, ignore.case=TRUE)
X<-gsub("^TZ$","Tanzania",X, ignore.case=TRUE)
X<-gsub("^UA$","Ukraine",X, ignore.case=TRUE)
X<-gsub("^UG$","Uganda",X, ignore.case=TRUE)
X<-gsub("^UM$","UnitedStatesMinorOutlyingIslands",X, ignore.case=TRUE)
X<-gsub("^US$","UnitedStatesofAmerica",X, ignore.case=TRUE)
X<-gsub("^UY$","Uruguay",X, ignore.case=TRUE)
X<-gsub("^UZ$","Uzbekistan",X, ignore.case=TRUE)
X<-gsub("^VA$","VaticanCity",X, ignore.case=TRUE)
X<-gsub("^VC$","SaintVincentandtheGrenadines",X, ignore.case=TRUE)
X<-gsub("^VE$","Venezuela",X, ignore.case=TRUE)
X<-gsub("^VG$","BritishVirginIslands",X, ignore.case=TRUE)
X<-gsub("^VI$","USVirginIslands",X, ignore.case=TRUE)
X<-gsub("^VN$","Vietnam",X, ignore.case=TRUE)
X<-gsub("^VU$","Vanuatu",X, ignore.case=TRUE)
X<-gsub("^WF$","WallisandFutuna",X, ignore.case=TRUE)
X<-gsub("^WS$","Samoa",X, ignore.case=TRUE)
X<-gsub("^YE$","Yemen",X, ignore.case=TRUE)
X<-gsub("^YT$","Mayotte",X, ignore.case=TRUE)
X<-gsub("^ZA$","SouthAfrica",X, ignore.case=TRUE)
X<-gsub("^ZM$","Zambia",X, ignore.case=TRUE)
X<-gsub("^ZW$","Zimbabwe",X, ignore.case=TRUE)
X<-gsub("\\,", "",X, ignore.case=TRUE)
worldcities2013$Country <- X
rm(X)

worldcities2013$Country <- tolower(worldcities2013$Country)

#Recoding city names to our standard
worldcities2013$City <- gsub(" ", "", worldcities2013$City)
worldcities2013$City <- tolower(worldcities2013$City)



###World cities dataset 2/2 (worldcities2009 from the 'maps' package)###

worldcities2009$name <- tolower(worldcities2009$name)

#the dataset has missing or wrong information
worldcities2009$capital[worldcities2009$name == "delhi" & worldcities2009$country.etc == "India"] <- "1"
worldcities2009$name[worldcities2009$name == "soul" & worldcities2009$country.etc == "Korea South"] <- "seoul"
worldcities2009$name[worldcities2009$name == "bombay" &  worldcities2009$country.etc  == "India"] <- "mumbai"
worldcities2009$name[worldcities2009$name == "new york" &  worldcities2009$country.etc  == "USA "] <- "newyorkcity"


#Reaching/Defining country and city standard
worldcities2009$country.etc[worldcities2009$country.etc == "Russia"] <- "Russian Federation"
worldcities2009$country.etc[worldcities2009$country.etc == "UK"] <- "United Kingdom"
worldcities2009$country.etc[worldcities2009$country.etc == "USA"] <- "United States of America"
worldcities2009$country.etc[worldcities2009$country.etc == "Korea North"] <- "Korea, Democratic People's Republic of"
worldcities2009$country.etc[worldcities2009$country.etc == "Korea South"] <- "Korea, Republic of"
worldcities2009$country.etc[worldcities2009$country.etc == "Sicily"] <- "Italy"
worldcities2009$country.etc[worldcities2009$country.etc == "East Timor"] <- "Timor-Leste"
worldcities2009$country.etc[worldcities2009$country.etc == "Madeira"] <- "Portugal"
worldcities2009$country.etc[worldcities2009$country.etc == "Madiera"] <- "Portugal"

worldcities2009$country.etc<-gsub(" ", "",worldcities2009$country.etc, ignore.case=TRUE)
worldcities2009$country.etc<-gsub("\\,", "",worldcities2009$country.etc, ignore.case=TRUE)
worldcities2009$country.etc <- tolower(worldcities2009$country.etc)

worldcities2009$name <- gsub(" ", "", worldcities2009$name)
worldcities2009$name <- tolower(worldcities2009$name)


###### Manipulate Data  ######


#We now merge the two datasets into one. First, some preparation.

#Ordering to later enable selection on duplicates
worldcities2013 <- worldcities2013[order(-worldcities2013$Population, na.last=TRUE) , ]
worldcities2009 <- worldcities2009[order(-worldcities2009$pop, na.last=TRUE) , ]

#Creating a subset with matching columns
worldcities2013 <- subset(worldcities2013, select =c("City", "Country", "Population", "Latitude", "Longitude", "Region"))
colnames(worldcities2013)[1] <- "name"
colnames(worldcities2013)[2] <- "country.etc"
colnames(worldcities2013)[3] <- "pop"
colnames(worldcities2013)[4] <- "lat"
colnames(worldcities2013)[5] <- "long"

#Creating a column to merge over: countrycity
worldcities2009$merge <- paste(worldcities2009$country.etc, worldcities2009$name, sep="")
worldcities2013$merge <- paste(worldcities2013$country.etc, worldcities2013$name, sep="")

#Merging to new set: "world.cities"
world.cities <- merge(worldcities2009, worldcities2013, by= c("merge", "name", "country.etc", "pop", "lat", "long"), all=TRUE)
world.cities <- world.cities[order(world.cities$merge, world.cities$capital, world.cities$pop),]
world.cities <- world.cities[!duplicated(world.cities$merge), ]
world.cities$merge <-NULL
world.cities <- world.cities[order(-world.cities$pop), ]
rm(worldcities2013, worldcities2009)

#Bringing country names in combined world.cities to WDI lowercase standard
world.cities$country.etc[world.cities$name == "berane"] <- "montenegro"
world.cities$country.etc[world.cities$name == "podgorica"] <- "montenegro"
world.cities$country.etc[world.cities$name == "niksic"] <- "montenegro"
world.cities$country.etc[world.cities$name == "pljevlja"] <- "montenegro"
world.cities$country.etc[world.cities$name == "budva"] <- "montenegro"
world.cities$country.etc[world.cities$name == "cetinje"] <- "montenegro"

world.cities$country.etc[world.cities$country.etc == "serbiaandmontenegro"] <- "serbia"
world.cities$country.etc[world.cities$country.etc == "norfolkisland"] <- "australia"
world.cities$country.etc[world.cities$country.etc == "bahamas"] <- "bahamasthe"
world.cities$country.etc[world.cities$country.etc == "brunei"] <- "bruneidarussalam"
world.cities$country.etc[world.cities$country.etc == "guernseyandalderney"] <- "channelislands"
world.cities$country.etc[world.cities$country.etc == "jersey"] <- "channelislands"
world.cities$country.etc[world.cities$country.etc == "easterisland"] <- "chile"
world.cities$country.etc[world.cities$country.etc == "taiwan"] <- "china"
world.cities$country.etc[world.cities$country.etc == "congodemocraticrepublic"] <- "congodemrep"
world.cities$country.etc[world.cities$country.etc == "congo"] <- "congorep"
world.cities$country.etc[world.cities$country.etc == "ivorycoast"] <- "cotedivoire"
world.cities$country.etc[world.cities$country.etc == "faroeislands"] <- "denmark"
world.cities$country.etc[world.cities$country.etc == "egypt"] <- "egyptarabrep"
world.cities$country.etc[world.cities$country.etc == "frenchguiana"] <- "france"
world.cities$country.etc[world.cities$country.etc == "frenchsouthernterritories"] <- "france"
world.cities$country.etc[world.cities$country.etc == "guadeloupe"] <- "france"
world.cities$country.etc[world.cities$country.etc == "martinique"] <- "france"
world.cities$country.etc[world.cities$country.etc == "mayotte"] <- "france"
world.cities$country.etc[world.cities$country.etc == "reunion"] <- "france"
world.cities$country.etc[world.cities$country.etc == "saint-barthelemy"] <- "france"
world.cities$country.etc[world.cities$country.etc == "saintpierreandmiquelon"] <- "france"
world.cities$country.etc[world.cities$country.etc == "wallisandfutuna"] <- "france"
world.cities$country.etc[world.cities$country.etc == "gambia"] <- "gambiathe"
world.cities$country.etc[world.cities$country.etc == "iran"] <- "iranislamicrep"
world.cities$country.etc[world.cities$country.etc == "koreademocraticpeople'srepublicof"] <- "koreademrep"
world.cities$country.etc[world.cities$country.etc == "korearepublicof"] <- "korearep"
world.cities$country.etc[world.cities$country.etc == "kyrgyzstan"] <- "kyrgyzrepublic"
world.cities$country.etc[world.cities$country.etc == "laos"] <- "laopdr"
world.cities$country.etc[world.cities$country.etc == "macedonia"] <- "macedoniafyr"
world.cities$country.etc[world.cities$country.etc == "micronesia"] <- "micronesiafedsts"
world.cities$country.etc[world.cities$country.etc == "netherlandsantilles"] <- "netherlands"
world.cities$country.etc[world.cities$country.etc == "cookislands"] <- "newzealand"
world.cities$country.etc[world.cities$country.etc == "niue"] <- "newzealand"
world.cities$country.etc[world.cities$country.etc == "tokelau"] <- "newzealand"
world.cities$country.etc[world.cities$country.etc == "svalbardandjanmayen"] <- "norway"
world.cities$country.etc[world.cities$country.etc == "azores"] <- "portugal"
world.cities$country.etc[world.cities$country.etc == "serbiaandmontenegro"] <- "serbia"
world.cities$country.etc[world.cities$country.etc == "slovakia"] <- "slovakrepublic"
world.cities$country.etc[world.cities$country.etc == "canaryislands"] <- "spain"
world.cities$country.etc[world.cities$country.etc == "westernsahara"] <- "spain"
world.cities$country.etc[world.cities$country.etc == "saintkittsandnevis"] <- "stkittsandnevis"
world.cities$country.etc[world.cities$country.etc == "saintlucia"] <- "stlucia"
world.cities$country.etc[world.cities$country.etc == "saint-martin"] <- "stmartinfrenchpart"
world.cities$country.etc[world.cities$country.etc == "saintvincentandthegrenadines"] <- "stvincentandthegrenadines"
world.cities$country.etc[world.cities$country.etc == "syria"] <- "syrianarabrepublic"
world.cities$country.etc[world.cities$country.etc == "turksandcaicos"] <- "turksandcaicosislands"
world.cities$country.etc[world.cities$country.etc == "anguilla"] <- "unitedkingdom"
world.cities$country.etc[world.cities$country.etc == "britishvirginislands"] <- "unitedkingdom"
world.cities$country.etc[world.cities$country.etc == "falklandislands"] <- "unitedkingdom"
world.cities$country.etc[world.cities$country.etc == "gibraltar"] <- "unitedkingdom"
world.cities$country.etc[world.cities$country.etc == "montserrat"] <- "unitedkingdom"
world.cities$country.etc[world.cities$country.etc == "pitcairn"] <- "unitedkingdom"
world.cities$country.etc[world.cities$country.etc == "sainthelena"] <- "unitedkingdom"
world.cities$country.etc[world.cities$country.etc == "northernmarianaislands"] <- "unitedstates"
world.cities$country.etc[world.cities$country.etc == "unitedstatesofamerica"] <- "unitedstates"
world.cities$country.etc[world.cities$country.etc == "venezuela"] <- "venezuelarb"
world.cities$country.etc[world.cities$country.etc == "usvirginislands"] <- "virginislandsus"
world.cities$country.etc[world.cities$country.etc == "palestine"] <- "westbankandgaza"
world.cities$country.etc[world.cities$country.etc == "yemen"] <- "yemenrep"

#On another checking process, we found other wrong information that needs correction.
world.cities$capital[world.cities$name == "newdelhi" &  world.cities$country.etc  == "india"] <- 1
world.cities$capital[world.cities$name == "peking" &  world.cities$country.etc  == "china"] <- 1
world.cities$capital[world.cities$name == "beirut" &  world.cities$country.etc  == "lebanon"] <- 1
world.cities$capital[world.cities$name == "guatemalacity" &  world.cities$country.etc  == "guatemala"] <- 1

X <- world.cities$country.etc
source('SmallScripts/CleanSpecialCharacters.R')
world.cities$country.etc <- X
rm(X)

#create cache worldcities.csv (last: 15.11.2014)
write.csv(world.cities, "Cache/world.cities.csv")
