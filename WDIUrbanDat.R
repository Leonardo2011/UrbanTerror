# Project Part Urban Population Data per Country
#The World Bank offers large datasets on information about the distribution of population. Most importantly, these include urban 
#population. The datasets are available for download on the World Banks Website (http://data.worldbank.org/). Though downloadable,
#data there is not presented in a tidy way. Vincent Arel-Bundocks' WDI package (https://github.com/vincentarelbundock/WDI) offers
#fuctions to scrape data from the World Bank's database in a tidy way.

install.packages("WDI")
library(WDI)

#With WDISearch(), we can look for a list of indicators we are interested in containing the character string specified as an 
#argument. Regular Experessions can be used. We are interested in a subset of all indicators that contain [Uu]rban.
#We now let the WDI() function create our dataframe with all the urban indicators we are interested in. We also include the 
#extra argument, which adds information that is typically presented together with the urban indicators.

WDIUrbanDat <- WDI(indicator=c('EN.POP.SLUM.UR.ZS', 
                              'EN.URB.LCTY.UR.ZS',
                              'EN.URB.MCTY',
                              'EN.URB.MCTY.TL.ZS',
                              'SP.URB.GROW', 
                              'SP.URB.TOTL', 
                              'SP.URB.TOTL.IN.ZS'),
                              country="all", start=1970, end=2013, extra=TRUE
                    )

#We leave the World Bank codings of the indicators unchanged. Though they would be easier human-readable if we used their full
#names, the codes are easier to google. They directly link to exact the definitions used by the World Bank. The codes stand for:
#EN.POP.SLUM.UR.ZS = Population living in slums, (% of urban population)
#EN.URB.LCTY.UR.ZS = Population in the largest city (% of urban population)
#EN.URB.MCTY = Population in urban agglomerations of more than 1 million
#EN.URB.MCTY.TL.ZS = Population in urban agglomerations of more than 1 million (% of total population)
#SP.URB.GROW = Urban population growth (annual %)
#SP.URB.TOTL = Urban population
#SP.URB.TOTL.IN.ZS = Urban population (% of total)
