

PreGTD <- read.csv('TerrorData/Pregtd.csv')
PreGTD$X <- NULL

PreGTD$PROPscale<-as.numeric(PreGTD$PROPscale)
PreGTD$HUMscale<-as.numeric(PreGTD$HUMscale)
PreGTD["weightGTD"] <- as.numeric((PreGTD$HUMscale^(0.5))+(PreGTD$PROPscale^(0.2)))
PreGTD$weightGTD[is.na(PreGTD$weightGTD)] <- 0
PreGTD$Rank01.C<-as.numeric(PreGTD$Rank01.W)
PreGTD$weightGTD<-as.numeric(PreGTD$weightGTD)

# linear regression of Rel.CS over time
Linear1 <- lm(100*Rel.CS ~ iyear, weight=weightGTD, data=subset(PreGTD,((Extra.WAR.In+Extra.WAR.Out+Intra.WAR+Inter.WAR==0) & iyear >=1990 & iyear <=2011 & (TUPscale!=(0|1|2|3)))))
summary(Linear1)
stargazer(Linear1, type="latex", out="Analysis Test/Linear Regression of realtive city size.html")



#summary(Linear1)
plot(Linear1, which = 1)
plot(PreGTD$iyear, 100*PreGTD$Rel.CS)


