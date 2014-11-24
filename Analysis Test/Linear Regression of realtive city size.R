


# selecting non-capital, non-military atacks
PreGTD2 <- subset(PreGTD, capital ==!1 & (TUPscale == 5 |TUPscale == 7 | TUPscale == 9))
PreGTD2$SP.URB.TOTL <- as.numeric(PreGTD2$SP.URB.TOTL)
PreGTD2$MAX.URB.TOTL <- as.numeric(PreGTD2$MAX.URB.TOTL)
PreGTD2$pop <- as.numeric(PreGTD2$pop)
PreGTD2 <- PreGTD2[order(-PreGTD2$eventid),]

# prepare for regression
PreGTD2["year"] <- as.numeric(PreGTD2$iyear)

PreGTD2["city.population_2013"] <- PreGTD2$pop

PreGTD2["city.population_with_time"] <- 
  
  ifelse((G$part.of.urban.center==FALSE&(G$largestC==1)&(!is.na(G$EN.URB.LCTY))), G$EN.URB.LCTY.UR, G$realpop)
  
  
  ((PreGTD2$pop)*(PreGTD2$SP.URB.TOTL)/(PreGTD2$MAX.URB.TOTL))










PreGTD2["Citysize_relative_to_nations_Largest" ] <- as.numeric(PreGTD2$city.population_at.attack.time_liniear.grown/PreGTD2$EN.URB.LCTY.UR)
PreGTD2["PROPscale" ]<-as.numeric(PreGTD2$PROPscale)

PreGTD2["HUMscale" ]<-as.numeric(PreGTD2$HUMscale)

PreGTD2["Distance to Urban Center (km)"]<-as.numeric(PreGTD2$WC.UC.dist.km)

PreGTD2["weightGTD"] <- as.numeric((PreGTD2$HUMscale^(1/2))+(PreGTD2$PROPscale/(PreGTD2$PROPscale+1)+1))
PreGTD2$Citysize_relative_to_nations_Largest[is.na(PreGTD2$Citysize_relative_to_nations_Largest)] <- 0
PreGTD2$weightGTD[is.na(PreGTD2$weightGTD)] <- 0

# linear regression of Citysize_relative_to_nations_Largest over time
Linear1 <- lm(Citysize_relative_to_nations_Largest ~ year, weight=weightGTD, data=PreGTD2)
stargazer(Linear1, type="latex", out="Analysis Test/Linear Regression of realtive city size.html")



#summary(Linear1)
#plot(Linear1, which = 1)
plot(PreGTD2$year, 100*PreGTD2$Citysize_relative_to_nations_Largest)




