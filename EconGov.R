# Lukas Brainstrom, things to think about testing:

# GDP, GDP Growth
# Numer of Minorities or languages spoken 
# Geographical Size, Population Density (Size/POP toal)
# Total Pop, Population Growth 
# Urban pop, Urban Pop Share
# Telephone Acess, Energy Access, Transportation Acces
# Infrastructure invenstment
# Roads to Pop density (Length of contry roads (+ train?) divided by Pop Density)
# Mil Expenditury 



EconGov <- WDI(indicator=c('NY.GDP.MKTP.KD', 
                           'NY.GDP.MKTP.KD.ZG',
                           'NY.GDP.MKTP.PP.KD',
                           'NY.GDP.PCAP.KD', 
                           'NY.GDP.PCAP.KD.ZG', 
                           'NY.GDP.PCAP.PP.KD',
                           'MS.MIL.XPND.ZS'),
               country="all", start=1970, end=2013, extra=FALSE,
)
#NY.GDP.MKTP.KD - GDP (constant 2000 US$)
#NY.GDP.MKTP.KD.ZG - GDP growth (annual%)
#NY.GDP.MKTP.PP.KD - GDP, PPP (constant 2005 international US$)
#NY.GDP.PCAP.KD - GDP/capita (constant 2000US$)
#NY.GDP.PCAP.KD.ZG - GDP/capita growth (annual %)
#NY.GDP.PCAP.PD.KD - GDP/capita, PPP (constant 2005 international US$)
#MS.MIL.XPND.ZS - Military Expenditure (% GDP)

EconGov <- subset(EconGov, 
                  EconGov$country != "Africa" &
                    EconGov$country != "Arab World" &
                    EconGov$country != "Andean Region" &
                    EconGov$country != "Caribbean small states" &         
                    EconGov$country != "Central Europe and the Baltics" &   
                    EconGov$country != "East Asia & Pacific (all income levels)" &   
                    EconGov$country != "East Asia & Pacific (developing only)" &   
                    EconGov$country != "East Asia and the Pacific (IFC classification)" &   
                    EconGov$country != "Euro area" &   
                    EconGov$country != "Europe & Central Asia (all income levels)" &   
                    EconGov$country != "Europe & Central Asia (developing only)" &   
                    EconGov$country != "Europe and Central Asia (IFC classification)" &   
                    EconGov$country != "European Union" &
                    EconGov$country != "Fragile and conflict affected situations" &
                    EconGov$country != "Heavily indebted poor countries (HIPC)" &
                    EconGov$country != "High income" &
                    EconGov$country != "High income: nonOECD" &
                    EconGov$country != "High income: OECD" &
                    EconGov$country != "Latin America & Caribbean (all income levels)" &
                    EconGov$country != "Latin America & Caribbean (developing only)" &
                    EconGov$country != "Latin America and the Caribbean" &
                    EconGov$country != "Latin America and the Caribbean (IFC classification)" &
                    EconGov$country != "Least developed countries: UN classification" &
                    EconGov$country != "Low & middle income" &
                    EconGov$country != "Low income" &
                    EconGov$country != "Lower middle income" &
                    EconGov$country != "Mexico and Central America" &
                    EconGov$country != "Middle East & North Africa (all income levels)" &
                    EconGov$country != "Middle East & North Africa (developing only)" &
                    EconGov$country != "Middle East and North Africa (IFC classification)" &
                    EconGov$country != "Middle income" &
                    EconGov$country != "North Africa" &
                    EconGov$country != "North America" &
                    EconGov$country != "Not classified" &
                    EconGov$country != "OECD members" &
                    EconGov$country != "Other small states" &
                    EconGov$country != "Pacific island small states" &
                    EconGov$country != "Small states" &
                    EconGov$country != "South Asia" &
                    EconGov$country != "South Asia (IFC classification)" &
                    EconGov$country != "Southern Cone Extended" &
                    EconGov$country != "Sub-Saharan Africa (all income levels)" &
                    EconGov$country != "Sub-Saharan Africa (developing only)" &
                    EconGov$country != "Sub-Saharan Africa (IFC classification)" &
                    EconGov$country != "Sub-Saharan Africa excluding South Africa" &
                    EconGov$country != "Sub-Saharan Africa excluding South Africa and Nigeria" &
                    EconGov$country != "Upper middle income" &
                    EconGov$country != "World"
)
EconGov <- EconGov[order(EconGov$country, EconGov$year), ]
