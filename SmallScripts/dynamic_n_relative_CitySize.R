# minor cleaning
PreGTD$part.of.urban.center[is.na(PreGTD$part.of.urban.center)] <- FALSE
PreGTD$in.urban.centers.environment[is.na(PreGTD$in.urban.centers.environment)] <- FALSE
PreGTD$in.urban.centers.environment <- recode(PreGTD$in.urban.centers.environment, "TRUE=1")
PreGTD$part.of.urban.center <- recode(PreGTD$part.of.urban.center, "TRUE=1")
PreGTD$capital[is.na(PreGTD$capital)] <- 0 
PreGTD$largestC[is.na(PreGTD$largestC)] <- 0 
PreGTD$largest.UC[is.na(PreGTD$largest.UC)] <- 0 
PreGTD$coastalMC[is.na(PreGTD$coastalMC)] <- 0 
PreGTD$pop[is.na(PreGTD$pop)] <- 0 
PreGTD$HUMscale[is.na(PreGTD$HUMscale)] <- 0
PreGTD$TUPscale[is.na(PreGTD$TUPscale)] <- 0

G2<-PreGTD
G2$SP.URB.TOTL <- as.numeric(G2$SP.URB.TOTL )
G2$MAX.URB.TOTL <- as.numeric(G2$MAX.URB.TOTL)
G2$EN.URB.MCTY <- as.numeric(G2$EN.URB.MCTY)
G2$MAX.URB.MCTY <- as.numeric(G2$MAX.URB.MCTY)
G2$EN.URB.LCTY.UR <- as.numeric(G2$EN.URB.LCTY.UR)
G2$MAX.URB.LCTY.UR <- as.numeric(G2$MAX.URB.LCTY.UR)
G2$pop <- as.numeric(G2$pop)


# prepare for regression
G2["year"] <- as.numeric(G2$iyear)
G2 <- G2[order(-G2$pop),]
G2["pop.today"] <- G2$pop

#in case we only have URB.POP numers, we assume that all cities grew with that
G2["city.population_with_time"] <- ifelse(!is.na(G2$SP.URB.TOTL), G2$pop.today*G2$SP.URB.TOTL/G2$MAX.URB.TOTL, G2$pop.today)

# if it is the largest city, EN.URB.LCTY.UR is the size
G2$city.population_with_time <-ifelse(G2$part.of.urban.center==FALSE
                                      & G2$pop.today>=999999
                                      & G2$largestC==1
                                      & !is.na(G2$EN.URB.LCTY.UR)|
                                        G2$part.of.urban.center==1
                                      & G2$largest.UC==1
                                      & !is.na(G2$EN.URB.LCTY.UR), G2$EN.URB.LCTY.UR, G2$city.population_with_time)


# if it is a city with more than 1mil, EN.URB.MCTY-EN.URB.LCTY.UR is the size
G2$city.population_with_time <- ifelse(G2$city.population_with_time!=G2$EN.URB.LCTY.UR
                                       & G2$pop.today>=999999
                                       & G2$EN.URB.LCTY.UR >= G2$EN.URB.MCTY
                                       & G2$MAX.URB.LCTY.UR >= G2$MAX.URB.MCTY
                                       & !is.na(G2$EN.URB.MCTY)
                                       & !is.na(G2$EN.URB.LCTY.UR)
                                       & !is.na(G2$MAX.URB.MCTY)
                                       & !is.na(G2$MAX.URB.LCTY.UR)
                                       & G2$MAX.URB.MCTY!=0, 
                                       ((G2$EN.URB.MCTY-G2$EN.URB.LCTY.UR)/
                                          (G2$MAX.URB.MCTY-G2$MAX.URB.LCTY.UR)
                                        *G2$city.population_with_time), (G2$city.population_with_time))

# if it is a city with less than 1mil, SP.URB.TOTL-EN.URB.MCTY is the size
G2$city.population_with_time <- ifelse(G2$city.population_with_time!=G2$EN.URB.LCTY.UR
                                       & G2$pop.today<=999999
                                       & G2$EN.URB.MCTY >= G2$SP.URB.TOTL
                                       & G2$MAX.URB.MCTY >= G2$MAX.URB.TOTL
                                       & !is.na(G2$EN.URB.MCTY)
                                       & !is.na(G2$MAX.URB.MCTY)
                                       & !is.na(G2$MAX.URB.TOTL)
                                       & G2$MAX.URB.TOTL!=0
                                       & G2$MAX.URB.MCTY!=0
                                       & !is.na(G2$EN.URB.LCTY.UR) 
                                       & !is.na(G2$SP.URB.TOTL),  ((G2$SP.URB.TOTL-G2$EN.URB.MCTY)/
                                                                     (G2$MAX.URB.TOTL-G2$MAX.URB.MCTY)
                                                                   *G2$city.population_with_time), (G2$city.population_with_time))


# some cleaning
G2$city.population_with_time <- ifelse(is.na(G2$city.population_with_time)&(!is.na(G2$pop.today)), G2$pop.today, 
                                       G2$city.population_with_time)
G2$EN.URB.LCTY.UR <- ifelse(is.na(G2$EN.URB.LCTY.UR)&(!is.na(G2$pop)), G2$pop, G2$EN.URB.LCTY.UR)
G2$EN.URB.LCTY.UR <- ifelse(is.na(G2$EN.URB.LCTY.UR)&(!is.na(G2$pop))&G2$part.of.urban.center==1
                            & G2$largest.UC==1, G2$pop, G2$EN.URB.LCTY.UR)
G2$city.population_with_time <- ifelse(is.infinite(G2$city.population_with_time), G2$pop.today, G2$city.population_with_time)


                                 

G2$pop <-  round(G2$city.population_with_time)
G2$pop[which(is.na(G2$pop))] <- 0
G2["Rel.CS"] <- G2$pop/G2$EN.URB.LCTY.UR
G2$Rel.CS[which(is.na(G2$Rel.CS))] <- 0
G2$Rel.CS <- ifelse(G2$Rel.CS>=1, 1,G2$Rel.CS)

PreGTD <- G2
rm(G2)