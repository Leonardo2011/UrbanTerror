########################################
############# URBAN TERROR #############
########################################
############  Part I: Data   ###########
########################################
#######Lukas B Cameron R Sascha S#######
########################################


########## Correlates of War ###########
########## IntraStatesWars #############

###### Gathering Data  ######

#We access and clean the Intrastates War Database
#Requires RCurl. If not done yet, either install the RCurl package or 
#run Packages.R from our repository


x <- getURL("www.correlatesofwar.org/COW2%20Data/WarData_NEW/Intra-StateWarData_v4.1.csv")
COWIS <- read.csv(text = x)

rm(x)
