

WC.UC.dist <- read.csv("WC.UC.dist.gis.csv", header=TRUE)

PreGTD <- read.csv('TerrorData/Pregtd.csv')

X <- subset(PreGTD, !is.na(GTD.city) & is.na(WCUC.city))
X$GTD.city <- sub("^$", NA, X$GTD.city)
X$GTD.city <- sub("^ $", NA, X$GTD.city)
X <- subset(X, !is.na(GTD.city))

X$X <- 1
List <- aggregate(X$X, by=list(X$GTD.city), FUN=sum)
List <- List[order(-List$x), ]
rm(X)
List <- merge(List, PreGTD, by.x=c("Group.1"), by.y=c("GTD.city"), all.x=TRUE)
List <- subset(List, is.na(List$old.pop))
List <- List[!duplicated(List$merge),]
List1 <- subset(List, x>3)
List2 <- subset(List, x==2|x==3)
List21 <- subset (List2, X<=median(X))
List2 <- subset (List2, !X<=median(X))
List3 <- subset(List, x==1)
List3a <- subset (List3, X<=median(X))
List3b <- subset (List3, !X<=median(X))
List31 <- subset (List3a, !X<=median(X))
List34 <- subset (List3a, X<=median(X))
List37 <- subset (List3b, !X<=median(X))
List39 <- subset (List3b, X<=median(X))
List32 <- subset (List31, !X<=median(X))
List31 <- subset (List31, X<=median(X))
List33 <- subset (List34, !X<=median(X))
List34 <- subset (List34, X<=median(X))
List35 <- subset (List37, !X<=median(X))
List36 <- subset (List37, X<=median(X))
List37 <- subset (List39, !X<=median(X))
List38 <- subset (List39, X<=median(X))
rm(List, List3, List3a, List3b, List39)

options(geonamesUsername="lokullustest")
options(geonamesUsername="loklok13")
#options(geonamesUsername="loklok14")

GNName1 <- paste(List1$original.city, List1$country_txt, sep=", ")
ListGN1 <- ldply(sapply(GNName1, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN1$merge <- ListGN1$.id
ListGN.small1 <- subset(ListGN1, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small1, file="missing.latlon.over1.csv")

GNName2 <- paste(List2$original.city, List2$country_txt, sep=", ")
ListGN2 <- ldply(sapply(GNName2, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN2$merge <- ListGN2$.id
ListGN.small2 <- subset(ListGN2, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small2, file="missing.latlon.over2.csv")

GNName3 <- paste(List31$original.city, List31$country_txt, sep=", ")
ListGN3 <- ldply(sapply(GNName3, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN3$merge <- ListGN3$.id
ListGN.small3 <- subset(ListGN3, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small3, file="missing.latlon.over3.csv")

GNName4 <- paste(List32$original.city, List32$country_txt, sep=", ")
ListGN4 <- ldply(sapply(GNName4, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN4$merge <- ListGN4$.id
ListGN.small4 <- subset(ListGN4, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small4, file="missing.latlon.over4.csv")

GNName5 <- paste(List33$original.city, List33$country_txt, sep=", ")
ListGN5 <- ldply(sapply(GNName5, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN5$merge <- ListGN5$.id
ListGN.small5 <- subset(ListGN5, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small5, file="missing.latlon.over5.csv")

GNName6 <- paste(List34$original.city, List34$country_txt, sep=", ")
ListGN6 <- ldply(sapply(GNName6, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN6$merge <- ListGN6$.id
ListGN.small6 <- subset(ListGN6, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small6, file="missing.latlon.over6.csv")

GNName7 <- paste(List35$original.city, List35$country_txt, sep=", ")
ListGN7 <- ldply(sapply(GNName7, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN7$merge <- ListGN7$.id
ListGN.small7 <- subset(ListGN7, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small7, file="missing.latlon.over7.csv")

GNName8 <- paste(List36$original.city, List36$country_txt, sep=", ")
ListGN8 <- ldply(sapply(GNName8, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN8$merge <- ListGN8$.id
ListGN.small8 <- subset(ListGN8, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small8, file="missing.latlon.over8.csv")

GNName9 <- paste(List37$original.city, List37$country_txt, sep=", ")
ListGN9 <- ldply(sapply(GNName9, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN9$merge <- ListGN9$.id
ListGN.small9 <- subset(ListGN9, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small9, file="missing.latlon.over9.csv")

GNName10 <- paste(List38$original.city, List38$country_txt, sep=", ")
ListGN10 <- ldply(sapply(GNName10, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN10$merge <- ListGN10$.id
ListGN.small10 <- subset(ListGN10, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small10, file="missing.latlon.over10.csv")

GNName11 <- paste(List21$original.city, List21$country_txt, sep=", ")
ListGN11 <- ldply(sapply(GNName11, function (x) {(GNsearch(maxRows="1", featureClass="P", orderby="relevence", q=x))}), data.frame)
ListGN11$merge <- ListGN11$.id
ListGN.small11 <- subset(ListGN11, select=c("merge", "lng", "lat", "population"))                
write.csv(ListGN.small11, file="missing.latlon.over11.csv")


Missings <- join_all(list(ListGN1, ListGN2, ListGN3, ListGN4, ListGN10,ListGN9,ListGN8, ListGN7, ListGN6, ListGN5, ListGN11), type = 'full')
Missings$gtd.city <- gsub(", .*$", "", Missings$merge)
Missings$gtd.country <- gsub("^.*, ", "", Missings$merge)
Missings$found.name <- Missings$name
Missings <- subset(Missings, select=c("gtd.country", "gtd.city", "found.name", "population", "fcodeName", "lat", "lng", "merge")) 
Missings$population <- as.numeric(Missings$population)
Missings <- Missings[order(-Missings$population), ]

Missings$non.city.loc <- tolower(Missings$gtd.city)
 Missings$non.city.loc <- sub(".*.*province.*.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*district.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*community.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*commune.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*dist$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*prov$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*island$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*coast$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*ship$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*boat$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*hills$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*area$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*border$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*hwy$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*dept$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*nationwide.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*valley$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*department$.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*state.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*beach.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*westbank.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*negev.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*desert.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*forest.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*sansalvador$", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*northcacharhills.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*corsica$.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*between.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*road.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*highway.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*province.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*municipality.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*station.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*forwardoperatingbase.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*rwanda.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*russianfederation.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*iraq.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*unitedstates.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*nepal.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*mozambique.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*algeria.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*burundi.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*angola.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*bangladesh.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*sudan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*nicaragua.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*lebanon.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*india.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*indonesia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*srilanka.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*pakistan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*punjab.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*westbankandgaza.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*yemenrep.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*afghanistan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*egyptarabrep.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*elsalvador.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*nigeria.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*syrianarabrepublic.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*cambodia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*chad.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*china.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*colombia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*tunisia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*ethiopia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*norway.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*israel.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*southafrica.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*southsudan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*djibouti.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*uganda.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*unitedarabemirates.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*congorep.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*saudiarabia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*sierraleone.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*guatemala.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*malaysia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*spain.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*philippines.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*myanmar.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*greece.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*kenya.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*peru.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*unitedkingdom.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*vietnam.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*turkey.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*guinea.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*barbados.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*iranislamicrep.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*azerbaijan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*somalia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*congodemrep.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*mali.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*korearep.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*armenia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*kosovo.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*senegal.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*morocco.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*namibia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*thailand.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*germany.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*hongkongsarchina.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*mexico.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*mauritania.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*honduras.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*niger.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*moldova.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*bolivia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*zambia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*centralafricanrepublic.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*georgia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*cameroon.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*uzbekistan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*zimbabwe.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*grenada.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*guineabissau.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*venezuelarb.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*eritrea.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*tajikistan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*jamaica.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*france.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*kuwait.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*cotedivoire.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*newcaledonia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*libya.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*macaosarchina.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*paraguay.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*guyana.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*macedoniafyr.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*brazil.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*chile.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*haiti.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*laopdr.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*lesotho.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*suriname.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*albania.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*ecuador.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*liberia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*ukraine.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*bosniaandherzegovina.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*papuanewguinea.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*bahrain.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*ireland.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*switzerland.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*bhutan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*botswana.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*italy.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*lithuania.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*timorleste.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*cyprus.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*jordan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*kyrgyzrepublic.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*poland.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*serbia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*sovietunion.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*cuba.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*dominicanrepublic.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*gabon.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*madagascar.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*panama.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*austria.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*costarica.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*ghana.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*kazakhstan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*netherlands.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*qatar.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*togo.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*yugoslavia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*argentina.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*hungary.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*malawi.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*portugal.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*puertorico.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*slovakrepublic.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*solomonislands.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*sweden.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*tanzania.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*trinidadandtobago.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*bulgaria.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*canada.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*comoros.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*croatia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*czechrepublic.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*equatorialguinea.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*estonia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*japan.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*latvia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*luxembourg.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*montenegro.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*antiguaandbarbuda.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*australia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*bahamasthe.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*belarus.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*belgium.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*belize.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*benin.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*denmark.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*fiji.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*iceland.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*slovenia.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*swaziland.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*vanuatu.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*kilometer.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*mountains.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*airbase.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*distcrict.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*highlands.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*hills.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*regimiento.*", 1, Missings$non.city.loc)
 Missings$non.city.loc <- sub(".*junction.*", 1, Missings$non.city.loc)
Missings$non.city.loc[Missings$gtd.city == "Arunachal Pradesh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Assam" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bihar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Chhattisgarh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Goa" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Gujarat" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Haryana" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Himachal Pradesh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Jammu and Kashmir" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Jharkhand" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Karnataka" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kerala" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Madhya Pradesh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Maharashtra" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Manipur" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Meghalaya" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Mizoram" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nagaland" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Odisha" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Punjab" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Rajasthan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sikkim" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Tamil Nadu" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Telangana" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Tripura" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Uttar Pradesh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Uttarakhand" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "West Bengal" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Punjab" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sindh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Khyber Pakhtunkhwa" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Balochistan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "changwat" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Al Anbar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Babil" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Orissa" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Dhi Qar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Diyala" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Tharaka-Nithi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Taita-Taveta" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Makueni" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nyandarua" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kirinyaga" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Maysan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nineveh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Saladin" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sulaymaniyah" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Wasit " ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Governorate" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Badakhshan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Badghis" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Baghlan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Balkh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Daykundi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Ghor" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Helmand" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Jowzjan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kapisa" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kunar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Laghman" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Logar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Maidan Wardak" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nangarhar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nimruz" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nuristan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Paktia" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Paktika" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Panjshir" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Parwan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Takhar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Urozgan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Zabul" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Aceh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bali" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Babel" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Banten" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Java" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Jateng" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kalteng" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sulteng" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Jatim" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kaltim" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sumatra" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sulawesi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kalimantan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nusa Tenggara" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Maluku" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kaltara" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Malukuutara" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sulut" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sumut" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Papua" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Riau" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kepri" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sultra" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kalsel" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sulsel" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sumsel" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Jabar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kalbar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Papuabarat" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sulbar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sumbar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Tenggara" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Papua" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Luzon" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Visayas" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Mindanao" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Antioquia" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bolívar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Boyaca" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Caldas" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Cauca" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Cundinamarca" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Huila" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Guajira" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Narino" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Santander" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sucre" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Tolima" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Valle" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Naama" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "El Bayadh" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Laghouat" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Ouargla" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "El Oued" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Ghardaia" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Tamanrasset " ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Adrar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Tindouf" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bechar" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Turkana" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Pokot" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Samburu" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Trans Nzoia" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Uasin Gishu" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Elgeyo" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nandi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Baringo" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Laikipia" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Marakwet" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Taita" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Taveta" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Tharaka" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nithi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Abia" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Adamawa" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Akwa Ibom" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Anambra" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bauchi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bayelsa" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Benue" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Borno" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Cross River" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Delta" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Ebonyi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Edo" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Ekiti" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Enugu" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Gombe" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Imo" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Jigawa" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kaduna" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kano" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Katsina" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kebbi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kogi" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kwara" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Lagos" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nassarawa" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Niger" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Ogun" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Ondo" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Osun" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Oyo" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Plateau" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Rivers" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sokoto" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Taraba" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Yobe" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Zamfara" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Siberia" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Basso " ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Medio Giuba" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Ghedo" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bai" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bakool" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Basso Scebeli" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Banaadir" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Medio Scebeli" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Hiiraan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Galguduud" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Mudug" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Nugaal" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Bari" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sool" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sanaag" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Togdheer" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Woqooyi Galbeed" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Awdal" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Giuba" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Selangor" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sabah" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Sarawak" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Perak" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kedah" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Penang" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Kelantan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Pahang" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Terengganu" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Negeri Sembilan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Perlis" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Labuan" ] <- 1
Missings$non.city.loc[Missings$gtd.city == "Putrajaya" ] <- 1



Missings$population[Missings$non.city.loc==1] <- 0
Missings$non.city.loc[Missings$non.city.loc!=1] <- 0

Missings$stringdist <- stringdist(Missings$found.name, Missings$gtd.city, method="jaccard") 
save.Missings <- subset(Missings, Missings$stringdist<=0.68 | Missings$non.city.loc==1)
Missings  <- subset(Missings, Missings$stringdist>0.68)



