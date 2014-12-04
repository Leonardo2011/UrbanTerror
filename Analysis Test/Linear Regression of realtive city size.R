

PreGTD <- read.csv('TerrorData/Pregtd.csv')


PreGTD$PROPscale<-as.numeric(PreGTD$PROPscale)
PreGTD$HUMscale<-as.numeric(PreGTD$HUMscale)
PreGTD["weightGTD"] <- as.numeric((PreGTD$HUMscale^(0.2))+(PreGTD$PROPscale^(0.1)))
PreGTD$weightGTD[is.na(PreGTD$weightGTD)] <- 0
PreGTD$Rank01.C<-as.numeric(PreGTD$Rank01.C)
PreGTD$weightGTD<-as.numeric(PreGTD$weightGTD)
PreGTD$country_txt <- as.factor(PreGTD$country_txt)

# linear regression of Rel.CS over time
#Linear1 <- lm(100*Rank01.C ~ iyear, weight=weightGTD, data=subset(PreGTD,((Extra.WAR.In+Extra.WAR.Out+Intra.WAR+Inter.WAR==0) & iyear >=1990 & iyear <=2011 & (TUPscale!=(0|1|2|3)))))
#summary(Linear1)
#stargazer(Linear1, type="latex", out="Analysis Test/Linear Regression of realtive city size.html")


earlyGTD <- subset(PreGTD, (Extra.WAR.In+Extra.WAR.Out+Intra.WAR+Inter.WAR==0) & 
                   iyear >=1970 & iyear <=1997 & TUPscale!=(0|1|2|3) & capital!=1)

earlyGTD   <- earlyGTD[order(earlyGTD$region_txt),]
earlyregionalGTD <- split(earlyGTD , earlyGTD $region_txt)

model.early.per.region <- lapply(earlyregionalGTD, function (x) {lm(100*Rank01.C ~ iyear, weight=weightGTD, na.action=na.exclude , data = x)})

stargazer(model.early.per.region, type="latex", out="Analysis Test/model.per.region.1970bis1997.html", 
          title="Regression 1970-1997 excluding Capitals, Countries during War, and Non-Urban Targets", align=TRUE, column.labels=c("Australa-Oceania","Central America", 
          "Central Asia","East Asia", "Eastern Europe","Arab World","North America","Russia-Ex-USSR","South America","South Asia",
          "Southeast Asia","Sub-Saharan Africa", "Western Europe"), covariate.labels=c("Time"), dep.var.labels=c("Relative City Rank", no.space=TRUE))
          


lateGTD <- subset(PreGTD, (Extra.WAR.In+Extra.WAR.Out+Intra.WAR+Inter.WAR==0) & 
                     iyear >=1998& iyear <=2011 & TUPscale!=(0|1|2|3) & capital!=1)

lateGTD   <- lateGTD[order(lateGTD$region_txt),]
lateregionalGTD <- split(lateGTD , lateGTD $region_txt)

model.late.per.region <- lapply(lateregionalGTD, function (x) {lm(100*Rank01.C ~ iyear, weight=weightGTD, na.action=na.exclude , data = x)})

stargazer(model.late.per.region, type="latex", out="Analysis Test/model.per.region.1998bis2011.html",
          title="Regression 1998-2011 excluding Capitals, Countries during War, and Non-Urban Targets", align=TRUE, column.labels=c("Australa-Oceania","Central America", 
                                                                                                                                    "Central Asia","East Asia", "Eastern Europe","Arab World","North America","Russia-Ex-USSR","South America","South Asia",
                                                                                                                                    "Southeast Asia","Sub-Saharan Africa", "Western Europe"), covariate.labels=c("Time"), dep.var.labels=c("Relative City Rank", no.space=TRUE))






