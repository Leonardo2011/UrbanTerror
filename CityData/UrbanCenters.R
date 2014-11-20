############# URBAN TERROR #############
########################################
############  Part I: Data   ###########
########################################
#######Lukas B Cameron R Sascha S#######
########################################


##### Urban Centers #####

#We use a Wikipedia article on global Urban Centers for our analysis on the geographical distance from the location of attacks
#to the nearest, "important" city. We then use the google.maps API to geolocate each of the centers.


###### Gathering Data  ######



# Scrap Wiki on Urban Centers using the 'XML' package
URL <- 'http://en.wikipedia.org/w/index.php?title=List_of_urban_areas_by_population&section=2'
table <- readHTMLTable(URL, encoding = "UTF-16")
UrbanCenters <- table [[2]]
UrbanCenters$City <- as.character(UrbanCenters$City)
colnames(UrbanCenters)[5] <- "Area"
colnames(UrbanCenters)[6] <- "Density"


##### Cleaning Data #####


#Cleaning the Urban Centers name in order to allow google.maps API to find them
UrbanCenters$City <- gsub("\\xc3\xa1","a", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xa2","a", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xa3","a", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xa4","a", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xa5","a", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xa0","a", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xa8","e", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xa9","e", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xaa","e", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xab","e", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xb1","n", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xb2","o", perl=TRUE, UrbanCenters$City) 
UrbanCenters$City <- gsub("\\xc3\xb3","o", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xb4","o", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xb5","o", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xb6","o", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xb9","u", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xba","u", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xbb","u", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xbc","u", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xac","i", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xad","i", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xae","i", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\xaf","i", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x80","A", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x81","A", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x82","A", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x83","A", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x84","A", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x85","A", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x88","E", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x89","E", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x8a","E", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x8b","E", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x8c","I", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x8d","I", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x8e","I", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x8f","I", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x91","N", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x92","O", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x93","O", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x94","O", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x95","O", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x98","O", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x99","U", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x9a","U", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x9c","U", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\xc3\x9b","U", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("\\(.*","", UrbanCenters$City)
UrbanCenters$City <- gsub("\\[.+?\\]","", UrbanCenters$City)
UrbanCenters$City <- gsub("\\(.+?\\)","", UrbanCenters$City)
UrbanCenters$City <- gsub("[[:digit:]]", "", UrbanCenters$City)
UrbanCenters$City <- gsub("\\xe2\x80\x93.*","", perl=TRUE, UrbanCenters$City)
UrbanCenters$City <- gsub("Region", "", UrbanCenters$City)
UrbanCenters$City <- gsub("Greater ", "", UrbanCenters$City)


###### Manipulate Data ######

#Putting in a string with "Country, City" to allow google.maps API to find them
CoCi <-data.frame(paste(UrbanCenters$Country, UrbanCenters$City, sep=", "), row.names = NULL)
CoCi["loc"] <- CoCi
CoCi$loc <- as.character(CoCi$loc)
CoCi$loc <- gsub("^..", "", CoCi$loc)
CoCiLoc<-(CoCi$loc)

#Looking up lon/lat data for each Urban Center via google.maps using the geocode function of the package maps.
#This is a very time consuming process, which is why we chose to cache the result to reduce computation time for this script.
if(file.exists("Cache/UrbanLoc.csv"))
  {UrbanLoc <- read.csv("Cache/UrbanLoc.csv")
  } else
  {
    UrbanLoc <- geocode(CoCiLoc, output = c("latlon", "latlona", "more", "all"),messaging = FALSE, sensor = FALSE, override_limit = FALSE)
    write.csv(UrbanLoc, "Cache/UrbanLoc.csv")
  }

#Inlcuding geographic data into the original Urban Centers data frame
UrbanCenters["lat"] <- UrbanLoc$lat
UrbanCenters["lon"] <- UrbanLoc$lon
UrbanCenters["full name"] <- CoCiLoc
UrbanCenters$City <- tolower(UrbanCenters$City)


#Including dummy variables for coastal megacities

###############If not sure, add here#########################
#UrbanCenters$coastalMC[UrbanCenters$City == "cairo"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "kolkata"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "bankok"] <- "1" # bangkok?
#UrbanCenters$coastalMC[UrbanCenters$City == "dhaka"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "london"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "hangzhou"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "houston"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "porto alegre"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "melbourne"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "sapporo"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "guayaquil"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "johor bahru"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "stockholm"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "changshu"] <- "1"
#UrbanCenters$coastalMC[UrbanCenters$City == "merida"] <- "1"
##############################################################



#####Main Set######
UrbanCenters$coastalMC=0
UrbanCenters$coastalMC[UrbanCenters$City == "tokyo"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "jakarta"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "seoul"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "shanghai"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "manila"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "karachi"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "new york city"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "sao paolo"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "mumbai"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "osaka"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "los angeles"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "buenos aires"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "istanbul"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "shenzen"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "lagos"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "rio de janeiro"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "nagoya"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "lima"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "chennai"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "chicago"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "hong kong"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "toronto"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "san francisco"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "miami"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "quanzhou"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "luanda"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "singapore"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "saint petersburg"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "surat"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "surabaya"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "abidjan"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "alexandria"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "barcelona"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "qingdao"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "suzhou"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "salvador"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "sydney"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "accra"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "recife"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "kuwait city"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "dalian"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "naples"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "dar es salaam"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "fuzhou"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "fortaleza"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "cape town"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "athens"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "durban"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "wuxi"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "tel aviv"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "jeddah"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "xiamen"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "busan"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "dubai"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "ningbo"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "dakar"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "seattle"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "chittagong"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "san diego"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "casablanca"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "algiers"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "taichung"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "izmir"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "wenzhou"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "kaohsiung"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "lisbon"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "maputo"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "douala"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "fukoka"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "tampa"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "tunis"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "cebu city"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "shantou"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "port-au-prince"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "zhongshan"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "santo domingo"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "medan"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "baltimore"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "kochi"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "panama city"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "colombo"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "vancouver"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "san juan"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "kozhikode"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "belem"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "baku"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "havana"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "tangshan"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "kitakyushu"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "beirut"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "port harcourt"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "barranquilla"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "brisbane"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "portland"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "rabat"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "lome"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "semarang"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "conakry"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "tijuana"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "visakhapatnam"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "thiruvananthapuram"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "santos"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "vitoria"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "montevideo"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "perth"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "kannur"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "mogadishu"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "haikou"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "makassar"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "marseille"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "davao city"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "virginia beach norfolk"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "porto"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "yantai"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "natal"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "sendai"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "huizhou"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "sao luis"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "hiroshima"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "auckland"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "zhuhai"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "tainan"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "maceio"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "copenhagen"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "cixi city"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "joao pessoa"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "zhangzhou"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "providence"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "tripoli"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "vasai virar"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "helsinki"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "putian"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "dublin"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "kollam"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "jacksonville"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "ulsan"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "haifa"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "florianopolis"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "mombasa"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "dongying"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "amsterdam"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "jiaxing"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "nantong"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "odessa"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "cartagena"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "hamamatsu"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "port elizabeth"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "changwon"] <- "1"
UrbanCenters$coastalMC[UrbanCenters$City == "monrovia"] <- "1"

# There are 2 Valencias. The one in Venezuela is not a CMC, just the one in Spain.
UrbanCenters$coastalMC[UrbanCenters$City == "valencia" & UrbanCenters$Country == "Spain"] <- "1" 

#Deleting what is not needed anymore
rm(UrbanLoc, table ,URL, CoCi, CoCiLoc)

#Caching the Urban Centers dataframe
write.csv(UrbanCenters, "Cache/UrbanCenters.csv")
