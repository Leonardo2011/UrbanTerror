
#download a limited GTD we created for this assignment (5MB instead of 100MB file)
#PreGTD_in_Memory <- getURL("https://rawgit.com/LBRETZIN/UrbanTerror/master/TerrorData/Pregtd.csv", ssl.verifypeer=0L, followlocation=1L)
#writeLines(PreGTD_in_Memory,'Pre.GTD.csv')
#rm(PreGTD_in_Memory)

#Load the Pre-Analysis Global Terrorism Database
PreGTD <- read.csv("TerrorData/Pregtd.csv", header=TRUE)
PreGTD <-PreGTD[order(-PreGTD$eventid, na.last=TRUE) , ]



# take out what we need
Stat1GTD <- subset(PreGTD, select=c(iyear, country_txt, region_txt, pop.that.year, Rel.CS, inUC, aroundUC, Rank01.C, 
                                  Rank01.W, capital, largestC, largest.UC, WC.UC.dist.km, TUPscale, PROPscale, HUMscale, 
                                  Extra.WAR.In, Extra.WAR.Out, Intra.WAR, Inter.WAR, coast.dist, coast.dist.MIN, coast.dist.MAX, access, access.MAX,
                                  light, light.MAX, nldi, nldi.MAX, urbn.cover, city.gdp, gdp.MAX, density, density.MAX, 
                                  density.growth, density.growth.MAX, EN.URB.MCTY.TL.ZS, SP.URB.TOTL.IN.ZS, EN.URB.LCTY.UR.ZS), iyear>=1998)

Stat1GTD["DV.Target.Urban"] <- (Stat1GTD$Rank01.C*0.25)  + (Stat1GTD$Rel.CS*0.25) + (Stat1GTD$urbn.cover/100*0.25) + (Stat1GTD$light/Stat1GTD$light.MAX*0.25)
Stat1GTD["DV.Target.Crowded"] <-(Stat1GTD$density/Stat1GTD$density.MAX*0.60) + (Stat1GTD$density.growth/Stat1GTD$density.growth.MAX*0.30) + (Stat1GTD$nldi/Stat1GTD$nldi.MAX*0.10)
Stat1GTD["DV.Target.Coastal"] <- ifelse(Stat1GTD$coast.dist.MIN >= 30, NA, Stat1GTD$coast.dist/Stat1GTD$coast.dist.MAX)
Stat1GTD["DV.Target.Connected"] <- (((Stat1GTD$access/Stat1GTD$access.MAX)-1)*-1)*0.5 + (Stat1GTD$light/Stat1GTD$light.MAX*0.25) + (Stat1GTD$city.gdp/Stat1GTD$gdp.MAX*0.25)
  
Stat1GTD["IV.Time"] <- Stat1GTD$iyear-1998
Stat1GTD["IV.Urban.Share"] <- Stat1GTD$SP.URB.TOTL.IN.ZS
Stat1GTD["IV.Urban.Center.Share"] <- Stat1GTD$EN.URB.MCTY.TL.ZS


