



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

#bosniaandherzegovina latvia switzerland azerbaijan kuwait netherlands venezuelarb kyrgyzrepublic liberia czechrepublic fiji
#guatemala kazakhstan belgium paraguay albania ireland timorleste croatia macaosarchina qatar bhutan  brazil
#stlucia trinidadandtobago maldives mozambique ecuador armenia austria finland slovakrepublic sweden bolivia
#djibouti bulgaria chile malaysia yugoslavia benin equatorialguinea hungary montenegro poland solomonislands zambia
#bahrain denmark nicaragua swaziland estonia gambiathe libya madagascar argentina costarica cuba jamaica japan lesotho
#malawi panama puertorico romania turkmenistan unitedarabemirates belize cyprus newzealand papuanewguinea portugal
#slovenia togo uruguay vietnam

Stat1GTD["one"] <- 1
Acount <- aggregate(one ~ country_txt,Stat1GTD,FUN=sum)
Acount <- Acount[order(-Acount$one, na.last=TRUE) , ]
Acount["Count1"] <- ifelse(Acount$one>= 20, 1, 0)
Acount$one <- NULL

#cameroon, guyana, timorleste, tunisia, kuwait, latvia, newzealand, paraguay, bolivia, congodemrep, fiji, guineabissau, 
#kazakhstan, madagascar, belarus, cyprus, eritrea, malaysia, norway, swaziland, bahrain, czechrepublic, hungary, macaosarchina,
#bhutan, hongkongsarchina, montenegro, mozambique, puertorico, slovenia, estonia, libya, slovakrepublic, solomonislands, vietnam, 
#denmark, korearep, maldives, moldova, panama, poland, qatar, trinidadandtobago, finland, lesotho, nicaragua, papuanewguinea, 
#portugal, unitedarabemirates, belize, benin, costarica, cuba, djibouti, equatorialguinea, gambiathe, jamaica, malawi, romania, 
#stlucia, togo, turkmenistan, uruguay

Stat1GTD <- merge(Stat1GTD, Asum, by=c("country_txt"), all.x=TRUE)
Stat1GTD <- merge(Stat1GTD, Acount, by=c("country_txt"), all.x=TRUE)
Stat1GTD <- subset(Stat1GTD, Count1==1 & Victim1==1)
Stat1GTD$one  <- NULL
Stat1GTD$Victim1 <- NULL
Stat1GTD$Count1 <- NULL
rm(Acount, Asum)


# Now we take aggregated means (country-years), not without putting a weight on the aggregation (Victims^04 + Damage^02))                              
DT <- data.table(Stat1GTD)
DVurb <- data.frame(DT[,list(DV.Target.Urban = weighted.mean(DV.Target.Urban, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcro <- data.frame(DT[,list(DV.Target.Crowded = weighted.mean(DV.Target.Crowded, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcoa <- data.frame(DT[,list(DV.Target.Coastal = weighted.mean(DV.Target.Coastal, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
DVcon <- data.frame(DT[,list(DV.Target.Connected = weighted.mean(DV.Target.Connected, (HUMscale^0.4 + PROPscale^0.2), rm.na=TRUE)),by=list(country_txt, iyear)])
IVdst <- data.frame(DT[,list(IV.Pop.Coastal.Dist = mean(Pop.Coast.Dist, rm.na=TRUE)),by=list(country_txt, iyear)])
VaR <- merge(DVurb, DVcro, by=c("country_txt", "iyear"), all=TRUE)
VaR <- merge(VaR, DVcoa, by=c("country_txt", "iyear"), all=TRUE)
VaR <- merge(VaR, DVcon, by=c("country_txt", "iyear"), all=TRUE)
VaR <- merge(VaR, IVdst, by=c("country_txt", "iyear"), all=TRUE)
VaR$IV.Pop.Coastal.Dist[is.na(VaR$DV.Target.Coastal)] <- NA
rm(DT, DVurb, DVcro, DVcoa, DVcon, IVdst)


### Fifth Dependent Variable: >Kilcullen< 
# The combination of all four variables, or three in case of landlocked countries                (not time variant)
VaR["DV.Kilcullen"] <- ifelse(is.na(VaR$DV.Target.Coastal), 
                              ((VaR$DV.Target.Urban+VaR$DV.Target.Crowded+VaR$DV.Target.Connected)/3),
                              (VaR$DV.Target.Urban+VaR$DV.Target.Crowded+VaR$DV.Target.Connected+VaR$DV.Target.Coastal)/4)


### include the WDI data
WDIData <- WDI(indicator=c('SP.URB.TOTL.IN.ZS'),country="all", start=1970, end=2013, extra=FALSE)
WDIData <- WDIData[order(WDIData$country, WDIData$year), ]
X <- WDIData$country
source('SmallScripts/CleanSpecialCharacters.R')
WDIData$country <- X

VaR <- merge(VaR, WDIData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE)
VaR$iso2c <- as.factor(VaR$iso2c)
colnames(VaR)[colnames(VaR) == "country_txt"] <- "country"
colnames(VaR)[colnames(VaR) == "SP.URB.TOTL.IN.ZS"] <- "IV.Urban.Share"
VaR$country <- as.factor(VaR$country)

VaR$IV.Time <- (VaR$iyear -1998)
VaR$IV.Urban.Share_Year <- VaR$IV.Time*VaR$IV.Urban.Share
VaR$IV.Pop.Coastal.Dist_Year <- VaR$IV.Time*VaR$IV.Pop.Coastal.Dist

write.csv(VaR, file="Analysis Test/Data/Analysis.Variables.csv")


gleich<-VaR 
#gleich <- read.csv("Analysis Test/Data/Analysis.Variables.csv", header=TRUE)


attach(gleich)
##least squares dummary variable model
fixed.dum1 <- lm(DV.Target.Urban ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year + factor(country) -1, data=gleich) 
fixed.dum2 <- lm(DV.Target.Connected ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year + factor(country) -1, data=gleich) 
fixed.dum3 <- lm(DV.Target.Coastal ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year + factor(country) -1, data=gleich) 
fixed.dum4 <- lm(DV.Target.Crowded ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year + factor(country) -1, data=gleich) 
fixed.dum5 <- lm(DV.Kilcullen ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year + factor(country) -1, data=gleich) 

stargazer(fixed.dum1,fixed.dum2,fixed.dum3,fixed.dum4,fixed.dum5,
          out="Analysis Test/5x5Panesl_least_squares_fixed.html",type="html", keep=c("IV.Urban.Share", "IV.Time", "IV.Urban.Share_Year",
                                                                                     "IV.Pop.Coastal.Dist", "IV.Pop.Coastal.Dist_Year"))


##least squares dummary variable model
fixed.dum1 <- lm(DV.Target.Urban ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year, data=gleich) 
fixed.dum2 <- lm(DV.Target.Connected ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year, data=gleich) 
fixed.dum3 <- lm(DV.Target.Coastal ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year, data=gleich) 
fixed.dum4 <- lm(DV.Target.Crowded ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year, data=gleich) 
fixed.dum5 <- lm(DV.Kilcullen ~ IV.Time + IV.Urban.Share + IV.Urban.Share_Year + IV.Pop.Coastal.Dist + 
                   IV.Pop.Coastal.Dist_Year, data=gleich) 

stargazer(fixed.dum1,fixed.dum2,fixed.dum3,fixed.dum4,fixed.dum5,
          out="Analysis Test/5x5Panesl_least_squares_notfixed5.html",type="html", keep=c("IV.Urban.Share", "IV.Time", "IV.Urban.Share_Year",
                                                                                         "IV.Pop.Coastal.Dist", "IV.Pop.Coastal.Dist_Year"))







