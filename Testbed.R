#testbed

X <- GTD$city
source('CityCleaning.R')

Y <- gsub(" ", "", GTD$country_txt)
Y <- tolower(Y)
Y <-gsub("\\,", "",Y, ignore.case=TRUE)

Testframe <-data.frame(paste(Y, X, sep=""), row.names = NULL)
