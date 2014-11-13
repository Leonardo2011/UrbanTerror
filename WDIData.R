# Project Part Urban Population Data per Country
#The World Bank offers large datasets on information about the distribution of population. Most importantly, these include urban 
#population. The datasets are available for download on the World Banks Website (http://data.worldbank.org/). Though downloadable,
#data there is not presented in a tidy way. Vincent Arel-Bundocks' WDI package (https://github.com/vincentarelbundock/WDI) offers
#fuctions to scrape data from the World Bank's database in a tidy way. If not done as of now, either install the WDI package or 
#run Data.Cleaning.R from our repository

#With WDISearch(), we can look for a list of indicators we are interested in containing the character string specified as an 
#argument. Regular Experessions can be used. We are interested in a subset of all indicators that contain [Uu]rban.
#We now let the WDI() function create our dataframe with all the urban indicators we are interested in. We also include the 
#extra argument, which adds information that is typically presented together with the urban indicators. Next we order the data
#first by country, then by year.

WDIData <- WDI(indicator=c('EN.URB.LCTY.UR.ZS',
                              'EN.URB.MCTY',
                              'EN.URB.MCTY.TL.ZS',
                              'SP.URB.GROW', 
                              'SP.URB.TOTL', 
                              'SP.POP.TOTL', 
                              'SP.URB.TOTL.IN.ZS',
                              'EN.POP.DNST',
                              'EN.RUR.DNST',
                              'SP.RUR.TOTL',
                              'SP.RUR.TOTL.ZG',
                              'SP.RUR.TOTL.ZS'),
                              country="all", start=1970, end=2013, extra=FALSE,
                    )
WDIData <- WDIData[order(WDIData$country, WDIData$year), ]




#We leave the World Bank codings of the indicators unchanged. Though they would be easier human-readable if we used their full
#names, the codes are easier to google. They directly link to exact the definitions used by the World Bank. The codes stand for:
#EN.URB.LCTY.UR.ZS = Population in the largest city (% of urban population)
#EN.URB.MCTY = Population in urban agglomerations of more than 1 million
#EN.URB.MCTY.TL.ZS = Population in urban agglomerations of more than 1 million (% of total population)
#SP.URB.GROW = Urban population growth (annual %)
#SP.URB.TOTL = Urban population
#SP.POP.TOTL = Total population
#SP.URB.TOTL.IN.ZS = Urban population (% of total)
#EN.POP.DNST = Population density (people per sq. km of land area)
#EN.RUR.DNST = Rural population density (rural population per sq. km of arable land)
#SP.RUR.TOTL = Rural population                                                         
#SP.RUR.TOTL.ZG = Rural population growth (annual %)                                       
#SP.RUR.TOTL.ZS = Rural population (% of total population)

#Introducing EN.URB.LCTY.UR = Population in the largest city
WDIData["EN.URB.LCTY.UR"] <- (WDIData$SP.POP.TOTL*WDIData$EN.URB.LCTY.UR.ZS)

# finding 3  maxima for each country (total Pop, Pop in Urban Cemters, Pop in largest City)
Highs.URB.TOTL <- aggregate(as.numeric(WDIData$SP.URB.TOTL), list(country=WDIData$country),max)
colnames(Highs.URB.TOTL)[2] <- "MAX.URB.TOTL"
Highs.URB.MCTY <- aggregate(as.numeric(WDIData$EN.URB.MCTY), list(country=WDIData$country),max)
colnames(Highs.URB.MCTY)[2] <- "MAX.URB.MCTY"
Highs.URB.LCTY.UR <- aggregate(as.numeric(WDIData$EN.URB.LCTY.UR), list(country=WDIData$country),max)
colnames(Highs.URB.LCTY.UR )[2] <- "MAX.URB.LCTY.UR"

# bringing it back into the WDI Data
WDIData <- merge(WDIData, Highs.URB.TOTL, by=("country"))
WDIData <- merge(WDIData, Highs.URB.MCTY, by=("country"))
WDIData <- merge(WDIData, Highs.URB.LCTY.UR, by=("country"))
rm(Highs.URB.TOTL, Highs.URB.MCTY, Highs.URB.LCTY.UR)


# Additional Info: We counted the amount of NAs across all dataframes.
#> sum(is.na(WDIUrbanDat$EN.URB.LCTY.UR.ZS))
#[1] 3519
#> sum(is.na(WDIUrbanDat$EN.URB.MCTY))
#[1] 6356
#> sum(is.na(WDIUrbanDat$EN.URB.MCTY.TL.ZS))
#[1] 5391
#> sum(is.na(WDIUrbanDat$SP.URB.GROW))
#[1] 675
#> sum(is.na(WDIUrbanDat$SP.URB.TOTL))
#[1] 671
#> sum(is.na(WDIUrbanDat$SP.URB.TOTL.IN.ZS))
#[1] 612
#> sum(is.na(WDIData$SP.URB.TOTL))
#[1] 886
#> sum(is.na(WDIData$EN.POP.DNST))
#[1] 986
#> sum(is.na(WDIData$EN.RUR.DNST))
#[1] 9112
#> sum(is.na(WDIData$SP.RUR.TOTL))
#[1] 886
#> sum(is.na(WDIData$SP.RUR.TOTL.ZG))
#[1] 1119
#> sum(is.na(WDIData$SP.RUR.TOTL.ZS))
#[1] 827
#> sum(is.na(WDIData$SP.POP.TOTL))
#[1] 798

#While for some of the variables we have pretty much complete information (or at least enough to extrapolate), 
#it does not look that good for other variables. We decided to drag them along for the time being anyway.
