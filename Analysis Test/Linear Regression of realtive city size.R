

PreGTD <- read.csv('TerrorData/Pregtd.csv')

PreGTD   <- PreGTD[order(-PreGTD$HUMscale),]

PreGTD$PROPscale<-as.numeric(PreGTD$PROPscale)
PreGTD$HUMscale<-as.numeric(PreGTD$HUMscale)
PreGTD["weightGTD"] <- as.numeric((PreGTD$HUMscale^(0.4))+(PreGTD$PROPscale^(0.1)))
PreGTD$weightGTD[is.na(PreGTD$weightGTD)] <- 0
PreGTD$Rank01.C<-as.numeric(PreGTD$Rank01.C)
PreGTD$weightGTD<-as.numeric(PreGTD$weightGTD)
PreGTD$country_txt <- as.factor(PreGTD$country_txt)
PreGTD$region_txt <- as.factor(PreGTD$region_txt)
PreGTD$Intra.WAR <- as.numeric(PreGTD$Intra.WAR)




#rank.model.per.region.1970bis1997: only external wars excluded, as are non-urban targets and capitals are in!
earlyGTD <- subset(PreGTD, (Extra.WAR.In+Inter.WAR==0) & 
                   iyear >=1970 & iyear <=1997 & TUPscale!=(0|1|2|3))
earlyGTD   <- earlyGTD[order(earlyGTD$region_txt),]
earlyregionalGTD <- split(earlyGTD, earlyGTD$region_txt)
earlyregionalGTD <- c(list(earlyGTD), earlyregionalGTD)
model.early.per.region <- lapply(earlyregionalGTD, function (x) {lm(100*Rank01.C ~ iyear, weight=weightGTD, na.action=na.exclude , data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/rank.model.per.region.1970bis1997.html", 
          title="Liniear Regression 1970-1997, excluding Countries in External War or Occupation, and Excluding Attacks on Non-Urban Targets", 
          align=TRUE, column.labels=c("World", "Australia-Oceania","Central America", "Central Asia","East Asia", "Eastern Europe","Arab World",
                                      "North America","Russia-Ex-USSR","South America","South Asia", "Southeast Asia","Sub-Saharan Africa",
                                      "Western Europe"), covariate.labels=c("Time"), dep.var.labels=c("Relative City Rank", no.space=TRUE))
rm(model.early.per.region, earlyregionalGTD, earlyGTD)
          

#rank.model.per.region.1998bis2013: only external wars excluded, as are non-urban targets and capitals are in!
lateGTD <- subset(PreGTD, (Extra.WAR.In+Inter.WAR==0) & 
                     iyear >=1998 & iyear <=2013 & TUPscale!=(0|1|2|3))
lateGTD   <- lateGTD[order(lateGTD$region_txt),]
lateregionalGTD <- split(lateGTD, lateGTD$region_txt)
lateregionalGTD <- c(list(lateGTD), lateregionalGTD)
model.late.per.region <- lapply(lateregionalGTD, function (x) {lm(100*Rank01.C ~ iyear, weight=weightGTD, na.action=na.exclude , data = x)})
stargazer(model.late.per.region, type="latex", out="Analysis Test/rank.model.per.region.1998bis2013.html",
          title="Liniar Regression by Region 1998-2013, excluding Countries in External War or Occupation, and Excluding Attacks on Non-Urban Targets", 
          align=TRUE, column.labels=c("World", "Australia-Oceania","Central America", "Central Asia","East Asia", "Eastern Europe","Arab World",
                                      "North America","Russia-Ex-USSR","South America","South Asia", "Southeast Asia","Sub-Saharan Africa",
                                      "Western Europe"), covariate.labels=c("Time"), dep.var.labels=c("Relative City Rank", no.space=TRUE))
rm(model.late.per.region, lateregionalGTD, lateGTD)




#rel.size.model.per.region.1970bis1997: only external wars excluded, as are non-urban targets and capitals are in!
earlyGTD <- subset(PreGTD, (Extra.WAR.In+Inter.WAR==0) & 
                     iyear >=1970 & iyear <=1997 & TUPscale!=(0|1|2|3))
earlyGTD   <- earlyGTD[order(earlyGTD$region_txt),]
earlyregionalGTD <-  split(earlyGTD, earlyGTD$region_txt)
earlyregionalGTD <- c(list(earlyGTD), earlyregionalGTD)
model.early.per.region <- lapply(earlyregionalGTD, function (x) {lm(100*Rel.CS ~ iyear, weight=weightGTD, na.action=na.exclude , data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/rel.size.model.per.region.1970bis1997.html", 
          title="Liniear Regression 1970-1997, excluding Countries in External War or Occupation, and Excluding Attacks on Non-Urban Targets", 
          align=TRUE, column.labels=c("World", "Australia-Oceania","Central America", "Central Asia","East Asia", "Eastern Europe","Arab World",
                                      "North America","Russia-Ex-USSR","South America","South Asia", "Southeast Asia","Sub-Saharan Africa",
                                      "Western Europe"), covariate.labels=c("Time"), dep.var.labels=c("Relative City Size", no.space=TRUE))
rm(model.early.per.region, earlyregionalGTD, earlyGTD)


#rel.size.model.per.region.1998bis2013: only external wars excluded, as are non-urban targets and capitals are in!
lateGTD <- subset(PreGTD, (Extra.WAR.In+Inter.WAR==0) & 
                    iyear >=1998 & iyear <=2013 & TUPscale!=(0|1|2|3))
lateGTD   <- lateGTD[order(lateGTD$region_txt),]
lateregionalGTD <- split(lateGTD , lateGTD $region_txt)
lateregionalGTD <- c(list(lateGTD), lateregionalGTD)
model.late.per.region <- lapply(lateregionalGTD, function (x) {lm(100*Rel.CS ~ iyear, weight=weightGTD, na.action=na.exclude , data = x)})
stargazer(model.late.per.region, type="latex", out="Analysis Test/rel.size.model.per.region.1998bis2013.html",
          title="Liniar Regression by Region 1998-2013, excluding Countries in External War or Occupation, and Excluding Attacks on Non-Urban Targets", 
          align=TRUE, column.labels=c("World", "Australia-Oceania","Central America", "Central Asia","East Asia", "Eastern Europe","Arab World",
                                      "North America","Russia-Ex-USSR","South America","South Asia", "Southeast Asia","Sub-Saharan Africa",
                                      "Western Europe"), covariate.labels=c("Time"), dep.var.labels=c("Relative City Size", no.space=TRUE))
rm(model.late.per.region, lateregionalGTD, lateGTD)


#rank.model.per.region.1970bis1997.INTRA.WAR.TEST: only external wars excluded, as are non-urban targets and capitals are in!
earlyGTD <- subset(PreGTD, (Extra.WAR.In+Inter.WAR==0) & 
                     iyear >=1970 & iyear <=1997 & TUPscale!=(0|1|2|3))
earlyGTD   <- earlyGTD[order(earlyGTD$region_txt),]
earlyregionalGTD <- split(earlyGTD , earlyGTD$region_txt)
earlyregionalGTD <- c(list(earlyGTD), earlyregionalGTD)
model.early.per.region <- lapply(earlyregionalGTD, function (x) {lm(100*Rank01.C ~ iyear + Intra.WAR + Intra.WAR*iyear, weight=weightGTD, na.action=na.exclude , data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/rank.model.per.region.1970bis1997.INTRA.WAR.TEST.html", 
          title="Liniear Regression 1970-1997, excluding Countries in External War or Occupation, and Excluding Attacks on Non-Urban Targets", 
          align=TRUE, column.labels=c("World", "Australia-Oceania","Central America", "Central Asia","East Asia", "Eastern Europe","Arab World",
                                      "North America","Russia-Ex-USSR","South America","South Asia", "Southeast Asia","Sub-Saharan Africa",
                                      "Western Europe"), covariate.labels=c("Time", "Civil War 0/1", "Time In Civil War"), dep.var.labels=c("Relative City Rank", no.space=TRUE))
rm(model.early.per.region, earlyregionalGTD, earlyGTD)


#rank.model.per.region.1998bis2013.INTRA.WAR.TEST: only external wars excluded, as are non-urban targets and capitals are in!
lateGTD <- subset(PreGTD, (Extra.WAR.In+Inter.WAR==0) & 
                    iyear >=1998 & iyear <=2013 & TUPscale!=(0|1|2|3))
lateGTD   <- lateGTD[order(lateGTD$region_txt),]
lateregionalGTD <- split(lateGTD , lateGTD $region_txt)
lateregionalGTD <- c(list(lateGTD), lateregionalGTD)
model.late.per.region <- lapply(lateregionalGTD, function (x) {lm(100*Rank01.C ~ iyear + Intra.WAR + Intra.WAR*iyear, weight=weightGTD, na.action=na.exclude , data = x)})
stargazer(model.late.per.region, type="latex", out="Analysis Test/rank.model.per.region.1998bis2013.INTRA.WAR.TEST.html",
          title="Liniar Regression by Region 1998-2013, excluding Countries in External War or Occupation, and Excluding Attacks on Non-Urban Targets", 
          align=TRUE, column.labels=c("World", "Australia-Oceania","Central America", "Central Asia","East Asia", "Eastern Europe","Arab World",
                                      "North America","Russia-Ex-USSR","South America","South Asia", "Southeast Asia","Sub-Saharan Africa",
                                      "Western Europe"), covariate.labels=c("Time", "Civil War 0/1", "Time In Civil War"), dep.var.labels=c("Relative City Rank", no.space=TRUE))
rm(model.late.per.region, lateregionalGTD, lateGTD)




#rel.size.model.per.region.1970bis1997.INTRA.WAR.TEST: only external wars excluded, as are non-urban targets and capitals are in!
earlyGTD <- subset(PreGTD, (Extra.WAR.In+Inter.WAR==0) & 
                     iyear >=1970 & iyear <=1997 & TUPscale!=(0|1|2|3))
earlyGTD   <- earlyGTD[order(earlyGTD$region_txt),]
earlyregionalGTD <- split(earlyGTD , earlyGTD $region_txt)
earlyregionalGTD <- c(list(earlyGTD), earlyregionalGTD)
model.early.per.region <- lapply(earlyregionalGTD, function (x) {lm(100*Rel.CS ~ iyear + Intra.WAR + Intra.WAR*iyear, weight=weightGTD, na.action=na.exclude , data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/rel.size.model.per.region.1970bis1997.INTRA.WAR.TEST.html", 
          title="Liniear Regression 1970-1997, excluding Countries in External War or Occupation, and Excluding Attacks on Non-Urban Targets", 
          align=TRUE, column.labels=c("World", "Australia-Oceania","Central America", "Central Asia","East Asia", "Eastern Europe","Arab World",
                                      "North America","Russia-Ex-USSR","South America","South Asia", "Southeast Asia","Sub-Saharan Africa",
                                      "Western Europe"), covariate.labels=c("Time", "Civil War 0/1", "Time In Civil War"), dep.var.labels=c("Relative City Size", no.space=TRUE))
rm(model.early.per.region, earlyregionalGTD, earlyGTD)


#rel.size.model.per.region.1998bis2013.INTRA.WAR.TEST: only external wars excluded, as are non-urban targets and capitals are in!
lateGTD <- subset(PreGTD, (Extra.WAR.In+Inter.WAR==0) & 
                    iyear >=1998 & iyear <=2013 & TUPscale!=(0|1|2|3))
lateGTD   <- lateGTD[order(lateGTD$region_txt),]
lateregionalGTD <- split(lateGTD , lateGTD $region_txt)
lateregionalGTD <- c(list(lateGTD), lateregionalGTD)
model.late.per.region <- lapply(lateregionalGTD, function (x) {lm(100*Rel.CS ~ iyear + Intra.WAR + I(Intra.WAR*iyear), weight=weightGTD, na.action=na.exclude , data = x)})
stargazer(model.late.per.region, type="latex", out="Analysis Test/rel.size.model.per.region.1998bis2013.INTRA.WAR.TEST.html",
          title="Liniar Regression by Region 1998-2013, excluding Countries in External War or Occupation, and Excluding Attacks on Non-Urban Targets", 
          align=TRUE, column.labels=c("World", "Australia-Oceania","Central America", "Central Asia","East Asia", "Eastern Europe","Arab World",
                                      "North America","Russia-Ex-USSR","South America","South Asia", "Southeast Asia","Sub-Saharan Africa",
                                      "Western Europe"), covariate.labels=c("Time", "Civil War 0/1", "Time In Civil War"), dep.var.labels=c("Relative City Size", no.space=TRUE))
rm(model.late.per.region, lateregionalGTD, lateGTD)





