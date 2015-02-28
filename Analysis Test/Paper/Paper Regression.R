



# Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("TerrorData/Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]


### take out what we need
Stat1GTD <- subset(PreGTD, select=c(iyear, country_txt, TUPscale, PROPscale, HUMscale, Extra.WAR.In, Extra.WAR.Out, Intra.WAR, 
                                    Inter.WAR, coast.dist, DV.Target.Urban, DV.Target.Crowded, DV.Target.Connected, DV.Target.Coastal,
                                    DV.Kilcullen, IV.Pop.Coastal.Dist, Pop.Coast.Dist, WCUC.city.old, merge), 
                   iyear>=1998 & iyear<=2013)
               

# Exclude countries with less than 20 casaulties or incidents
Asum <- aggregate(HUMscale ~ country_txt,Stat1GTD,FUN=sum)
Asum <- Asum[order(-Asum$HUMscale, na.last=TRUE) , ]
Asum["Victim1"] <- ifelse(Asum$HUMscale>= 5000, 1, 0)
Asum$HUMscale <- NULL
Stat1GTD["one"] <- 1
Acount <- aggregate(one ~ country_txt,Stat1GTD,FUN=sum)
Acount <- Acount[order(-Acount$one, na.last=TRUE) , ]
Acount["Count1"] <- ifelse(Acount$one>= 100, 1, 0)
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
rm(X, WDIData)


### Define some new variables
colnames(Stat1GTD)[colnames(Stat1GTD) == "SP.URB.TOTL.IN.ZS"] <- "IV.Urban.Share"
Stat1GTD$IV.Time <- (Stat1GTD$iyear -1998)
Stat1GTD$IV.Urban.Share_Year <- Stat1GTD$IV.Time*Stat1GTD$IV.Urban.Share
Stat1GTD$IV.Pop.Coastal.Dist_Year <- Stat1GTD$IV.Time*Stat1GTD$IV.Pop.Coastal.Dist
Stat1GTD["weightGTD"] <- as.numeric((Stat1GTD$HUMscale^(0.4))+(Stat1GTD$PROPscale^(0.1)))


# models see title
Stat1GTD   <- Stat1GTD[order(Stat1GTD$iso2c),]
topiso2cGDT <- split(Stat1GTD, Stat1GTD$iso2c)
topiso2cGDT <- c(list(Stat1GTD), topiso2cGDT)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(DV.Kilcullen ~ IV.Time, weight=weightGTD, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/top17_time.html", 
          title="Top 17 Targeted Countries: Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top17", "afghanistan", "colombia", "algeria", "israel", "india", "iraq", "kenya", "srilanka", 
          "nigeria","philippines","pakistan","russia","somalia","syria","thailand","yemen"),
          dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Time"),  align=TRUE,no.space=TRUE)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(DV.Kilcullen ~ IV.Urban.Share, weight=weightGTD, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/top17_urban.html", 
          title="Top 17 Targeted Countries: Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top17", "afghanistan", "colombia", "algeria", "israel", "india", "iraq", "kenya", "srilanka", 
                          "nigeria","philippines","pakistan","russia","somalia","syria","thailand","yemen"),
          dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Urban Population Share"),  align=TRUE,no.space=TRUE)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(DV.Kilcullen ~ IV.Urban.Share + IV.Urban.Share_Year , weight=weightGTD, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/top17_urban_interaction.html", 
          title="Top 17 Targeted Countries: Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top17", "afghanistan", "colombia", "algeria", "israel", "india", "iraq", "kenya", "srilanka", 
                          "nigeria","philippines","pakistan","russia","somalia","syria","thailand","yemen"),
          dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Urban Population Share", "Urban Population Share * Time"),  align=TRUE,no.space=TRUE)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(DV.Kilcullen ~ IV.Time + IV.Urban.Share, weight=weightGTD, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/top17_time_urban.html", 
          title="Top 17 Targeted Countries: Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top17", "afghanistan", "colombia", "algeria", "israel", "india", "iraq", "kenya", "srilanka", 
                          "nigeria","philippines","pakistan","russia","somalia","syria","thailand","yemen"),
          dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Time", "Urban Population Share"),  align=TRUE,no.space=TRUE)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(TUPscale ~ IV.Time + IV.Urban.Share, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/top17_TUPtime_urban.html", 
          title="Top 17 Targeted Countries: Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top17", "afghanistan", "colombia", "algeria", "israel", "india", "iraq", "kenya", "srilanka", 
                          "nigeria","philippines","pakistan","russia","somalia","syria","thailand","yemen"),
          dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Time", "Urban Population Share"),  align=TRUE,no.space=TRUE)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(TUPscale ~ DV.Kilcullen, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/top17_TUPkilcullen.html", 
          title="Top 17 Targeted Countries: Non-Government Targets / 1998-2013 (Linear Regression)", 
          column.labels=c("Top17", "afghanistan", "colombia", "algeria", "israel", "india", "iraq", "kenya", "srilanka", 
                          "nigeria","philippines","pakistan","russia","somalia","syria","thailand","yemen"),
          dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("DV Killy"),  align=TRUE,no.space=TRUE)



##########################################################
###########################################################
############################################################


Stat1GTD <- subset(PreGTD, select=c(eventid, iyear, country_txt, TUPscale, PROPscale, HUMscale, Extra.WAR.In, Extra.WAR.Out, Intra.WAR, 
                                    Inter.WAR, coast.dist, DV.Target.Urban, DV.Target.Crowded, DV.Target.Connected, DV.Target.Coastal,
                                    DV.Kilcullen, IV.Pop.Coastal.Dist, Pop.Coast.Dist, WCUC.city.old, merge), 
                   iyear>=1998 & iyear<=2013)


#Load the Global Terrorism Database (GTD). It is open souce and can be downloaded after registration at 
# http://www.start.umd.edu/gtd/contact/
unzip("TerrorData/globalterrorismdb_0814dist.zip", exdir="TerrorData")
rawGTD <- read.csv("TerrorData/globalterrorismdb_0814dist.csv", header=TRUE)
unlink("TerrorData/globalterrorismdb_0814dist.csv")

#The (GTD) contains over a 120k observations on more than 120 variables. Many seem irrelevant to our analysis. 
#We subset the database, selecting also only successful terror attacks.
#Documentation on the variables we select can be found on: http://www.start.umd.edu/gtd/downloads/Codebook.pdf

GTDg <- subset(rawGTD, select = c(eventid, gname), na.strings = c("", " "))
rm(rawGTD)

Stat1GTD <- merge(Stat1GTD, GTDg, by="eventid")
Stat1GTD$gname <- as.factor(Stat1GTD$gname)

# Exclude countries with less than 20 casaulties or incidents
Asum <- aggregate(HUMscale ~ gname ,Stat1GTD,FUN=sum)
Asum <- Asum[order(-Asum$HUMscale, na.last=TRUE) , ]
Asum["Victim1"] <- ifelse(Asum$HUMscale>= 1000, 1, 0)
Asum$HUMscale <- NULL
Stat1GTD["one"] <- 1
Acount <- aggregate(one ~ gname,Stat1GTD,FUN=sum)
Acount <- Acount[order(-Acount$one, na.last=TRUE) , ]
Acount["Count1"] <- ifelse(Acount$one>= 100, 1, 0)
Acount["Count1"] <- ifelse(Acount$gname=="Unknown" | Acount$gname=="Other" | Acount$gname=="Individual", 0, Acount$Count1)
Acount$one <- NULL
Stat1GTD <- merge(Stat1GTD, Asum, by=c("gname"), all.x=TRUE)
Stat1GTD <- merge(Stat1GTD, Acount, by=c("gname"), all.x=TRUE)
rm(Asum, Acount)
Stat1GTD <- subset(Stat1GTD, Count1==1 & Victim1==1)
Stat1GTD$one  <- NULL
Stat1GTD$Victim1 <- NULL
Stat1GTD$Count1 <- NULL

Stat1GTD$gname <- factor(Stat1GTD$gname)

### include the WDI data
WDIData <- WDI(indicator=c('SP.URB.TOTL.IN.ZS'),country="all", start=1970, end=2013, extra=FALSE)
WDIData <- WDIData[order(WDIData$country, WDIData$year), ]
X <- WDIData$country
source('SmallScripts/CleanSpecialCharacters.R')
WDIData$country <- X
Stat1GTD <- merge(Stat1GTD, WDIData, by.x=c("country_txt", "iyear"), by.y=c("country", "year"), all.x=TRUE)
Stat1GTD$iso2c <- as.factor(Stat1GTD$iso2c)
Stat1GTD$country <- as.factor(Stat1GTD$country)
rm(X, WDIData)


### Define some new variables
colnames(Stat1GTD)[colnames(Stat1GTD) == "SP.URB.TOTL.IN.ZS"] <- "IV.Urban.Share"
Stat1GTD$IV.Time <- (Stat1GTD$iyear -1998)
Stat1GTD$IV.Urban.Share_Year <- Stat1GTD$IV.Time*Stat1GTD$IV.Urban.Share
Stat1GTD$IV.Pop.Coastal.Dist_Year <- Stat1GTD$IV.Time*Stat1GTD$IV.Pop.Coastal.Dist
Stat1GTD["weightGTD"] <- as.numeric((Stat1GTD$HUMscale^(0.4))+(Stat1GTD$PROPscale^(0.1)))


# models see title
Stat1GTD   <- Stat1GTD[order(Stat1GTD$gname),]
topiso2cGDT <- split(Stat1GTD, Stat1GTD$gname)
topiso2cGDT <- c(list(Stat1GTD), topiso2cGDT)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(DV.Kilcullen ~ IV.Time, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/tog27_time.html", 
          title="Top 27 Groups: Non-Government Targets / 1998-2013 (Linear Regression)", 
          
          column.labels=c("Top27G", "Abu Sayyaf Group (ASG)", "Al-Aqsa Martyrs Brigade", "Al-Qa`ida in Iraq", 
                          "Al-Qa`ida in the Arabian Peninsula (AQAP)", "Al-Qa`ida in the Lands of the Islamic Maghreb (AQLIM)", 
                          "Al-Shabaab", "Algerian Islamic Extremists", "Armed Islamic Group (GIA)", "Boko Haram", 
                          "Chechen Rebels", "Communist Party of India - Maoist (CPI-Maoist)", 
                          "Hamas (Islamic Resistance Movement)", "Islamic State of Iraq (ISI)", 
                          "Islamic State of Iraq and the Levant", "Kurdistan Workers' Party (PKK)", 
                          "Lashkar-e-Taiba (LeT)", "Liberation Tigers of Tamil Eelam (LTTE)", "Lord's Resistance Army (LRA)", 
                          "Maoists", "Moro Islamic Liberation Front (MILF)", 
                          "National Union for the Total Independence of Angola (UNITA)",
                          "New People's Army (NPA)", "Revolutionary Armed Forces of Colombia (FARC)",
                          "Salafist Group for Preaching and Fighting (GSPC)", "Taliban", "Tehrik-i-Taliban Pakistan (TTP)", 
                          "United Liberation Front of Assam (ULFA)"),
          
          dep.var.labels=c("Urban, Networked, Crowded, Littoral Targets Location (% Points of Countries Maximum)"),
          covariate.labels=c("Time"),  align=TRUE,no.space=TRUE)




Stat1GTD <- subset(PreGTD, select=c(eventid, iyear, country_txt, TUPscale, PROPscale, HUMscale, Extra.WAR.In, Extra.WAR.Out, Intra.WAR, 
                                    Inter.WAR, coast.dist, DV.Target.Urban, DV.Target.Crowded, DV.Target.Connected, DV.Target.Coastal,
                                    DV.Kilcullen, IV.Pop.Coastal.Dist, Pop.Coast.Dist, WCUC.city.old, merge, pop.that.year, RANK.World, RANK.Country, inUC, capital), 
                   iyear>=1998 & iyear<=2013)



# Exclude countries with less than 20 casaulties or incidents
Csum <- aggregate(Stat1GTD$HUMscale, by=list(Stat1GTD$merge, Stat1GTD$iyear), FUN=sum)
colnames(Csum)[1] <- "merge"
colnames(Csum)[2] <- "iyear"
colnames(Csum)[3] <- "C.HUMscale"
Csum <- Csum[order(-Csum$C.HUMscale, na.last=TRUE) , ]
Stat1GTD["one"] <- 1
Ccount <- aggregate(Stat1GTD$one, by=list(Stat1GTD$merge, Stat1GTD$iyear), FUN=sum)
colnames(Ccount)[1] <- "merge"
colnames(Ccount)[2] <- "iyear"
colnames(Ccount)[3] <- "C.count"
Ccount <- Ccount[order(-Ccount$C.count, na.last=TRUE) , ]

Stat1GTD <- merge(Stat1GTD, Csum, by=c("merge", "iyear"), all.x=TRUE)
Stat1GTD <- merge(Stat1GTD, Ccount, by=c("merge", "iyear"), all.x=TRUE)

rm(Csum, Ccount)

Stat1GTD$CityGroups <- ifelse((Stat1GTD$inUC==1), "UC", "notUC")
CCsum <- aggregate(Stat1GTD$HUMscale, by=list(Stat1GTD$CityGroups, Stat1GTD$iyear), FUN=sum)
colnames(CCsum)[1] <- "merge"
colnames(CCsum)[2] <- "iyear"
colnames(CCsum)[3] <- "CC.HUMscale"
CCcount <- aggregate(Stat1GTD$one, by=list(Stat1GTD$CityGroups, Stat1GTD$iyear), FUN=sum)
colnames(CCcount)[1] <- "merge"
colnames(CCcount)[2] <- "iyear"
colnames(CCcount)[3] <- "CC.count"

Stat2GTD <- merge(CCcount, CCsum, by=c("merge", "iyear"), all=TRUE)




Stat2GTD$IV.Time <- (Stat2GTD$iyear -1998)
Stat1GTD["weightGTD"] <- as.numeric((Stat1GTD$HUMscale^(0.4))+(Stat1GTD$PROPscale^(0.1)))

Stat2GTD$merge <- factor(Stat2GTD$merge)


# models see title
Stat2GTD   <- Stat2GTD[order(Stat2GTD$merge),]
topiso2cGDT <- split(Stat2GTD, Stat2GTD$merge)
topiso2cGDT <- c(list(Stat2GTD), topiso2cGDT)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(CC.count ~ IV.Time, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/uccities_attackcount-time.html", 
          column.labels=c("Not.UC", "UC"),
          title="Top 100 cities: All Targets / 1998-2013 (Linear Regression)", 
          dep.var.labels=c("Number of Attacks"),
          covariate.labels=c("Time"),  align=TRUE,no.space=TRUE)

model.early.per.region <- lapply(topiso2cGDT, function (x) {lm(CC.HUMscale ~ IV.Time, data = x)})
stargazer(model.early.per.region, type="latex", out="Analysis Test/Paper/uc_attackvictims-time.html", 
          column.labels=c("Not.UC", "UC"),
          title="Top 100 cities: All Targets / 1998-2013 (Linear Regression)", 
          dep.var.labels=c("Number of Victims"),
          covariate.labels=c("Time"),  align=TRUE,no.space=TRUE)



TUT <- subset(PreGTD, inUC==1 & iyear>=1998 & coastalMC==1)
TUT <- aggregate(TUT$HUMscale, by=list(TUT$iyear), FUN=sum)

write.csv(TUT, "t.csv")
