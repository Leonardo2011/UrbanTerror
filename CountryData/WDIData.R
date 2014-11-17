############# URBAN TERROR #############
########################################
############  Part I: Data   ###########
########################################
#######Lukas B Cameron R Sascha S#######
########################################


##### World Development Indicators #####


###### Gathering Data  ######


#The World Bank offers large datasets on information about the distribution of population. Most importantly, these include urban 
#population. The datasets are available for download on the World Banks Website (http://data.worldbank.org/). Though downloadable,
#data there is not presented in a tidy way. Vincent Arel-Bundocks' WDI package (https://github.com/vincentarelbundock/WDI) offers
#fuctions to scrape data from the World Bank's database in a tidy way. If not done yet, either install the WDI package or 
#run Packages.R from our repository

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
#SP.URB.TOTL.IN.ZS = Urban population (% of total
#EN.POP.DNST = Population density (people per sq. km of land area)
#EN.RUR.DNST = Rural population density (rural population per sq. km of arable land)
#SP.RUR.TOTL = Rural population                                                         
#SP.RUR.TOTL.ZG = Rural population growth (annual %)                                       
#SP.RUR.TOTL.ZS = Rural population (% of total population)

###### Cleaning Data  ######

#Eliminating all Regions and other aggregation of countries.

WDIData <- subset(WDIData, 
                    WDIData$country != "Africa" &
                    WDIData$country != "Arab World" &
                    WDIData$country != "Andean Region" &
                    WDIData$country != "Caribbean small states" &         
                    WDIData$country != "Central Europe and the Baltics" &   
                    WDIData$country != "East Asia & Pacific (all income levels)" &   
                    WDIData$country != "East Asia & Pacific (developing only)" &   
                    WDIData$country != "East Asia and the Pacific (IFC classification)" &   
                    WDIData$country != "Euro area" &   
                    WDIData$country != "Europe & Central Asia (all income levels)" &   
                    WDIData$country != "Europe & Central Asia (developing only)" &   
                    WDIData$country != "Europe and Central Asia (IFC classification)" &   
                    WDIData$country != "European Union" &
                    WDIData$country != "Fragile and conflict affected situations" &
                    WDIData$country != "Heavily indebted poor countries (HIPC)" &
                    WDIData$country != "High income" &
                    WDIData$country != "High income: nonOECD" &
                    WDIData$country != "High income: OECD" &
                    WDIData$country != "Latin America & Caribbean (all income levels)" &
                    WDIData$country != "Latin America & Caribbean (developing only)" &
                    WDIData$country != "Latin America and the Caribbean" &
                    WDIData$country != "Latin America and the Caribbean (IFC classification)" &
                    WDIData$country != "Least developed countries: UN classification" &
                    WDIData$country != "Low & middle income" &
                    WDIData$country != "Low income" &
                    WDIData$country != "Lower middle income" &
                    WDIData$country != "Mexico and Central America" &
                    WDIData$country != "Middle East & North Africa (all income levels)" &
                    WDIData$country != "Middle East & North Africa (developing only)" &
                    WDIData$country != "Middle East and North Africa (IFC classification)" &
                    WDIData$country != "Middle income" &
                    WDIData$country != "North Africa" &
                    WDIData$country != "North America" &
                    WDIData$country != "Not classified" &
                    WDIData$country != "OECD members" &
                    WDIData$country != "Other small states" &
                    WDIData$country != "Pacific island small states" &
                    WDIData$country != "Small states" &
                    WDIData$country != "South Asia" &
                    WDIData$country != "South Asia (IFC classification)" &
                    WDIData$country != "Southern Cone Extended" &
                    WDIData$country != "Sub-Saharan Africa (all income levels)" &
                    WDIData$country != "Sub-Saharan Africa (developing only)" &
                    WDIData$country != "Sub-Saharan Africa (IFC classification)" &
                    WDIData$country != "Sub-Saharan Africa excluding South Africa" &
                    WDIData$country != "Sub-Saharan Africa excluding South Africa and Nigeria" &
                    WDIData$country != "Upper middle income" &
                    WDIData$country != "World"
)

X <- WDIData$country
source('SmallScripts/CleanSpecialCharacters.R')
WDIData$country <- X
rm(X)

#create cache WDIData.csv (last: 15.11.2014)
write.csv(WDIData, "Cache/WDIData.csv")
