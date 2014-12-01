

WC.UC.XXX <- read.csv("Cache/WC.UC.dist.old.csv")

# Country level data from the World Bank Development Indicators (WDI) and the The Correlates of War (COW) project data on wars.
if(file.exists("Cache/CountryData.csv")){CountryData <- read.csv("Cache/CountryData.csv")} else{source("1.b - Country Data.R")}


# bring the largest city from city data set into the county data set
LC <- aggregate(pop ~ country.etc,WC.UC.XXX,max)
colnames(LC)[2] <- "WC.UC.Largest"
CountryData <- merge(CountryData, LC, by.y=c("country.etc"), by.x=c("country"), all.x=TRUE)
rm(LC, WC.UC.XXX)


#replace empty EN.URB.LCTY.UR with largest city from city dataset and estimate population growth with coutry data if available
CountryData$EN.URB.LCTY.UR <-ifelse(is.na(CountryData$EN.URB.LCTY.UR) & !is.na(CountryData$SP.URB.TOTL), 
                 (CountryData$SP.URB.TOTL/CountryData$MAX.URB.TOTL*CountryData$WC.UC.Largest), CountryData$EN.URB.LCTY.UR)
CountryData$EN.URB.LCTY.UR <-ifelse(is.na(CountryData$EN.URB.LCTY.UR) & is.na(CountryData$SP.URB.TOTL), 
                           CountryData$WC.UC.Largest, CountryData$EN.URB.LCTY.UR)
CountryData$WC.UC.Largest <- NULL
CountryData$X <- NULL

#create cache WDIData.csv
write.csv(CountryData, "Cache/CountryData.csv")

