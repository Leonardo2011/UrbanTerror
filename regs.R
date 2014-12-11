ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#use packages
packages <- c("foreign", "car", "RCurl", "ggplot2", "WDI", "rtiff", "httr", "iterators", "dplyr", "plyr", "mapproj", 
              "stargazer", "rgdal", "sp", "dismo","XML", "maps", "ggmap", "Imap", "geonames", "GSIF", "tiff", "stats",
              "raster", "gdalUtils","geosphere", "maptools", "rgeos", "googleVis", "DataCombine", "R.utils", "data.table", "plm")

ipak(packages)
rm(packages, ipak)

PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/TerrorData/Pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(PreGTD_in_Memory,'Pregtd.csv')
rm(PreGTD_in_Memory)

#Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("TerrorData/Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]

install.packages("data.table")
library(data.table)
# take out what we need
Stat1GTD <- subset(PreGTD, select=c(eventid, iyear, country_txt, region_txt, pop.that.year, Rel.CS, inUC, aroundUC, Rank01.C, 
                                    Rank01.W, capital, largestC, largest.UC, WC.UC.dist.km, TUPscale, PROPscale, HUMscale, 
                                    Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR, coast.dist, coast.dist.MIN, coast.dist.MAX, 
                                    access, access.MAX, light, light.MAX, nldi, nldi.MAX, urbn.cover, city.gdp, gdp.MAX, density, 
                                    density.MAX, density.growth, density.growth.MAX, EN.URB.MCTY.TL.ZS, SP.URB.TOTL.IN.ZS, 
                                    EN.URB.LCTY.UR.ZS, latitude, longitude,  WCUC.city.old), iyear>=1998 & iyear<=2011)


# In order to build our varibales, we should exclude the influence of negative components, if there are any
nonnegative <- function(x) ifelse(x<0, 0, x)


### First Dependent Variable: >Urban< 
# 25% (Country relative) City Rank -> Urban as Log Size                             (time variant)
# 25% (Country relative) City Population -> Urban as Size                           (time variant)
# 25% (Country relative) Location's Urban Landcover -> Urban as Surface             (not time variant)
# 25% (Country relative) Location's Nightlights -> Urban as Life                    (not time variant, but data is out there)
Stat1GTD["DV.Target.Urban"] <- (nonnegative(Stat1GTD$Rank01.C*0.25)  
                                + nonnegative(Stat1GTD$Rel.CS*0.25) 
                                + nonnegative(Stat1GTD$urbn.cover/100*0.25)
                                + nonnegative(Stat1GTD$light/Stat1GTD$light.MAX*0.25))

### Second Dependent Variable: >Crowded< 
# 60% (Country relative) Location's Population Density -> Crowded as Density                        (time variant)
# 30% (Country relative) Location's Population Density Growth over past years -> Crowded as Growth  (time variant)
# 10% (Country relative) Night Light Development Index (NLDI) -> Crowded as Inequality              (not time variant)
Stat1GTD["DV.Target.Crowded"] <- (nonnegative(Stat1GTD$density/Stat1GTD$density.MAX*0.60) 
                                  + nonnegative(Stat1GTD$density.growth/Stat1GTD$density.growth.MAX*0.30) 
                                  + nonnegative(Stat1GTD$nldi/Stat1GTD$nldi.MAX*0.10))

### Third Dependent Variable: >Connected< 
# 50% (Country relative) Travel Distance to larger City -> Connected as Proximity to Larger Cities  (not time variant)
# 25% (Country relative) Travel Location's Nightlights -> Connected as Electrified                  (not time variant)
# 25% (Country relative) Locations GDP -> Connected as Productive                                   (not time variant)
Stat1GTD["DV.Target.Connected"] <- (nonnegative((((Stat1GTD$access/Stat1GTD$access.MAX)-1)*-1)*0.5) 
                                    + nonnegative(Stat1GTD$light/Stat1GTD$light.MAX*0.25) 
                                    + nonnegative(Stat1GTD$city.gdp/Stat1GTD$gdp.MAX*0.25))

### Forth Dependent Variable: >Coastal< 
# 100% (Country relative) Distance to the Coast, whereas landlocked countries are NA                (not time variant)
Stat1GTD["DV.Target.Coastal"] <- ifelse(Stat1GTD$coast.dist.MIN >= 30, NA, 
                                        nonnegative(((Stat1GTD$coast.dist/Stat1GTD$coast.dist.MAX)-1)*-1))
rm(nonnegative)

# Now we take aggregated means (country-years), not without putting a weight on the aggregation (Victims^04 + Damage^02))                              
DT <- data.table(Stat1GTD)
DVurb <- data.frame(DT[,list(DV.Target.Urban = weighted.mean(DV.Target.Urban, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcro <- data.frame(DT[,list(DV.Target.Crowded = weighted.mean(DV.Target.Crowded, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcoa <- data.frame(DT[,list(DV.Target.Coastal = weighted.mean(DV.Target.Coastal, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcon <- data.frame(DT[,list(DV.Target.Connected = weighted.mean(DV.Target.Connected, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
Dependent <- merge(DVurb, DVcro, by=c("country_txt", "iyear"), all=TRUE)
Dependent <- merge(Dependent, DVcoa, by=c("country_txt", "iyear"), all=TRUE)
Dependent <- merge(Dependent, DVcon, by=c("country_txt", "iyear"), all=TRUE)


rm(DT, DVurb, DVcro, DVcoa, DVcon)


###include the WDI data and clean it
install.packages("WDI")
library(WDI)
WDIData <- WDI(indicator=c('EN.URB.MCTY.TL.ZS',
                           'SP.URB.TOTL.IN.ZS'),
               country="all", start=1970, end=2013, extra=FALSE)

WDIData <- WDIData[order(WDIData$country, WDIData$year), ]

X <- WDIData$country
X <- as.character(X)
X <- gsub("\\,","",X)
X <- gsub("\\-","",X)
X <- gsub("\\-","",X)
X <- gsub("\\-","",X)
X <- gsub("\\'","",X)
X <- gsub("\\'","",X)
X <- gsub("\\.","",X)
X <- gsub("\\\\", "", X)
X <- gsub("\\(","",X)
X <- gsub("\\)","",X)
X<-gsub(" ","",X)
X <- tolower(X)
WDIData$country <- X


gleich <- merge(Dependent, WDIData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE)

library(plm)
gleich <- plm.data(gleich, index=c("country_txt","iyear"))
summary(gleich)

attach(gleich)
x1 <- cbind(EN.URB.MCTY.TL.ZS, SP.URB.TOTL.IN.ZS)
y1 <- cbind(DV.Target.Connected)  


###Model 1 - OLS
ols <-lm(y1 ~ x1, data=gleich)
summary(ols)

##try to plot, but cannot get it--does not matter
yhat <- ols$fitted
plot(gleich$iyear, gleich$y1, pch=19, xlab="x1", ylab="y1")
abline(lm(gleich$y1~gleich$iyear),lwd=3, col="red")


###Model 2
#Least squares dummy variable model...another way of using fixed effects, except by country too
fixed.dum <-lm(y1 ~ x1 + factor(country_txt) - 1, data=gleich)
summary(fixed.dum)

#try to plot with a fitted, but cannot get it--does not matter
yhat <- fixed.dum$fitted
library(car)
scatterplot(yhat~gleich$x1|gleich$country_txt, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(gleich$y1~gleich$x1),lwd=3, col="red")

##put them in a table ready for stargazer
install.packages("apsrtable")
library(apsrtable)
apsrtable(ols,fixed.dum, model.names = c("OLS", "OLS_DUM")) 

##
pFtest(fixed, ols) 


###Model 3
###Within estimator....aka one-way fixed effects
fixed<-plm(y1 ~ x1, index=c("country_txt", "iyear"), data=gleich, model="within")
summary(fixed)
#look at constants for each country
fixef(fixed)












#####Extra Notes#####
  
coplot(DV.Target.Urban ~ iyear|country_txt, type="l", Dependent)


library(foreign)
Panel <- WDIData

y<-Panel$EN.URB.LCTY.UR.ZS
x<-Panel$SP.URB.GROW
ols <-lm(y ~ year, data=Panel)
summary(ols)

library(car)
scatterplot(y~x, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)
scatterplot(y~year, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)


install.packages("plm")
library(plm)

attach(Panel)
X <- cbind(SP.URB.GROW, SP.URB.TOTL,  SP.POP.TOTL)

Y <- cbind(EN.URB.MCTY)

pdata<- plm.data(Panel, index=c("country","year"))

pooling <-plm(Y ~ X, data=pdata, model="pooling")
summary(pooling)

fixed<-plm(Y ~ X, data=pdata, model="within")
summary(fixed)
