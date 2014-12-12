



# Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("TerrorData/Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]


### take out what we need
Stat1GTD <- subset(PreGTD, select=c(iyear, country_txt, TUPscale, PROPscale, HUMscale, Extra.WAR.In, Extra.WAR.Out, Intra.WAR, 
                                    Inter.WAR, coast.dist, DV.Target.Urban, DV.Target.Crowded, DV.Target.Connected, DV.Target.Coastal,
                                    DV.Kilcullen, IV.Pop.Coastal.Dist, Pop.Coast.Dist, WCUC.city.old, merge), 
                   iyear>=1998 & iyear<=2013 & TUPscale!=(0|1|2|3) & Extra.WAR.In==0 & Inter.WAR==0 & Intra.WAR==0)
                   

# Exclude countries with less than 20 casaulties or incidents
Asum <- aggregate(HUMscale ~ country_txt,Stat1GTD,FUN=sum)
Asum <- Asum[order(-Asum$HUMscale, na.last=TRUE) , ]
Asum["Victim1"] <- ifelse(Asum$HUMscale>= 40, 1, 0)
Asum$HUMscale <- NULL
Stat1GTD["one"] <- 1
Acount <- aggregate(one ~ country_txt,Stat1GTD,FUN=sum)
Acount <- Acount[order(-Acount$one, na.last=TRUE) , ]
Acount["Count1"] <- ifelse(Acount$one>= 1000, 1, 0)
Acount$one <- NULL
Stat1GTD <- merge(Stat1GTD, Asum, by=c("country_txt"), all.x=TRUE)
Stat1GTD <- merge(Stat1GTD, Acount, by=c("country_txt"), all.x=TRUE)
rm(Asum, Acount)
Stat1GTD <- subset(Stat1GTD, Count1==1 & Victim1==1)
Stat1GTD$one  <- NULL
Stat1GTD$Victim1 <- NULL
Stat1GTD$Count1 <- NULL
	

### include the WDI data
WDIData <- WDI(indicator=c('SP.URB.TOTL.IN.ZS'),country="all", start=1970, end=2013, extra=FALSE)
WDIData <- WDIData[order(WDIData$country, WDIData$year), ]
X <- WDIData$country
source('SmallScripts/CleanSpecialCharacters.R')
WDIData$country <- X
Stat1GTD <- merge(Stat1GTD, WDIData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE)
Stat1GTD$iso2c <- as.factor(Stat1GTD$iso2c)
Stat1GTD$country <- as.factor(Stat1GTD$country)
rm(X)


### Define some new variables
colnames(Stat1GTD)[colnames(Stat1GTD) == "SP.URB.TOTL.IN.ZS"] <- "IV.Urban.Share"
Stat1GTD$IV.Time <- (Stat1GTD$iyear -1998)
Stat1GTD$IV.Urban.Share_Year <- Stat1GTD$IV.Time*Stat1GTD$IV.Urban.Share
Stat1GTD$IV.Pop.Coastal.Dist_Year <- Stat1GTD$IV.Time*Stat1GTD$IV.Pop.Coastal.Dist
Stat1GTD["weightGTD"] <- as.numeric((Stat1GTD$HUMscale^(0.4))+(Stat1GTD$PROPscale^(0.1)))


# models see title
Stat1GTD   <- Stat1GTD[order(Stat1GTD$iso2c),]
topiso2cGDT <- split(Stat1GTD, (Stat1GTD)$iso2c)
topiso2cGDT <- c(list(Stat1GTD), topiso2cGDT)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(DV.Kilcullen ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year, weight=weightGTD, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Regressions Top 10 Countries/Peace1998-2013.TIME&URBAN.html", 
          title="Top 10 Targeted Countries: Peacetime / Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top10", "Colombia", "Algeria", "India", "Nigeria", "Philippines", "Pakistan", "Russia", "Somalia", 
                          "Thailand","Yemen"), dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Time", "Urban Population Share", "Urban Population Share * Time"),  align=TRUE,no.space=TRUE)



model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(DV.Kilcullen ~ IV.Time, weight=weightGTD, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Regressions Top 10 Countries/Peace1998-2013.TIME.html", 
          title="Top 10 Targeted Countries: Peacetime / Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top10", "Colombia", "Algeria", "India", "Nigeria", "Philippines", "Pakistan", "Russia", "Somalia", 
                          "Thailand","Yemen"), dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Time"),  align=TRUE, no.space=TRUE,  omit.table.layout = "sn")


model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(DV.Kilcullen ~ IV.Time + IV.Pop.Coastal.Dist + IV.Pop.Coastal.Dist_Year, weight=weightGTD, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Regressions Top 10 Countries/Peace1998-2013.TIME&LITORAL.html", 
          title="Top 10 Targeted Countries: Peacetime / Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top10", "Colombia", "Algeria", "India", "Nigeria", "Philippines", "Pakistan", "Russia", "Somalia", 
                          "Thailand","Yemen"), dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Time", "Population's Distance to Coast", "Population's Distance to Coast * Time"),  align=TRUE,no.space=TRUE)


