
source("0 - Loading Packages.R")

#Load the Analysis Variables
gleich <- read.csv("Analysis Test/Data/Analysis.Variables.csv", header=TRUE)

library(plm)
gleich <- plm.data(gleich, index=c("country","iyear"))
summary(gleich)


m1 <- plm(y1 ~ wage + capital + lag(output,1) + capital:lag(output,1), 
          data = EmplUK, model="within", effect="twoway", index=c("firm","year"))

attach(gleich)
y1 <- cbind(DV.Target.Urban, DV.Target.Crowded)  
wint1<-lm(y1 ~ IV.Urban.Share+iyear+IV.Urban.Share:iyear, data=gleich)
summary(int1)

library(stargazer)
stargazer(ols, fixed, fixed.dum, type="html")


x1 <- cbind(iyear, IV.Urban.Share, int1)

###Model 1 - OLS
ols <-lm(y1 ~ x1, data=gleich)
summary(ols)

##try to plot, but cannot get it--does not matter
yhat <- ols$fitted
plot(iyear, y1, pch=19, xlab="x1", ylab="y1")
abline(lm(y1~iyear),lwd=3, col="red")

###Model 2
#Least squares dummy variable model...another way of using fixed effects, except by country too
fixed.dum <-lm(y1 ~ x1 + factor(country) - 1, data=gleich)
summary(fixed.dum)

#try to plot with a fitted, but cannot get it--does not matter
attach(gleich)
yhat <- fixed.dum$fitted
library(car)
scatterplot(yhat~x1|country, boxplots=FALSE, xlab="x1", ylab="yhat",smooth=FALSE)
abline(lm(y1~x1),lwd=3, col="red")

##put them in a table ready for stargazer
install.packages("apsrtable")
library(apsrtable)
apsrtable(ols,fixed.dum, model.names = c("OLS", "OLS_DUM")) 

##
pFtest(fixed, ols) 


###Model 3
###Within estimator....aka one-way fixed effects
fixed<-plm(y1 ~ x1, data=gleich, model="within")
summary(fixed)
#look at constants for each country
fixef(fixed)












#####Extra Notes#####
  
coplot(DV.Target.Urban ~ iyear|country_txt, type="l", Dependent)


library(foreign)
Panel <- WDIData

y<-Panel$EN.URB.LCTY.UR.ZS
x<-Panel$SP.URB.GROW
ols <-lm(y ~ year, data=Panel)
summary(ols)

library(car)
scatterplot(y~x, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)
scatterplot(y~year, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=Panel)


install.packages("plm")
library(plm)

attach(Panel)
X <- cbind(SP.URB.GROW, SP.URB.TOTL,  SP.POP.TOTL)

Y <- cbind(EN.URB.MCTY)

pdata<- plm.data(Panel, index=c("country","year"))

pooling <-plm(Y ~ X, data=pdata, model="pooling")
summary(pooling)

fixed<-plm(Y ~ X, data=pdata, model="within")
summary(fixed)
