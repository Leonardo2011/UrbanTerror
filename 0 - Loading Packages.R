# MPP-E1180: Introduction to Collaborative Social Science Data Analysis
### Fall 2014
### Instructor: Christopher Gandrud

##############################################
############## URBAN TERROR ##################
##############################################
#############  Part 0: PACKAGES   ############
##############################################
##########Lukas B Cameron R Sascha S##########
##############################################
########## Loading Needed Packages ###########
##############################################


##########  Package Loading  ###########


#Loading all required packages for the UrbanTerror Project using @stevenworthington's ipak.R gist from https://gist.github.com/stevenworthington/3178163.

# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("foreign", "car", "RCurl", "ggplot2", "WDI", "rtiff", "httr", "iterators", "dplyr", "plyr", "mapproj", 
              "stargazer", "rgdal", "sp", "dismo","XML", "maps", "ggmap", "Imap", "geonames", "GSIF", "tiff",
              "raster", "gdalUtils","geosphere", "maptools", "rgeos", "foreach", "DataCombine", "R.utils")

ipak(packages)
rm(packages, ipak)

