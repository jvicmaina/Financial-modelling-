require("timeSeries")
library(fGarch)
library(rugarch)
library(rmgarch)
library(quantmod)
library(GetQuandlData)
library(tidyverse)
#settting working directory

setwd("C:/Users/jvicm/Desktop/New folder/GARCH")

#Getting data
startDate =as.Date("2010-12-03")
endDate =as.Date("2022-12-03")
getSymbols("IBM", from= startDate, to =endDate)
getSymbols("GOOG", from= startDate, to =endDate)
getSymbols("BP", from= startDate, to =endDate )
#Looking at the data structure
head(IBM)
str(IBM)

par("mar")
par(mar=c(1,1,1,1))
par
chartSeries(GOOG)
tail(IBM)
 #working with returns to estimate volatility models
rIBM <- dailyReturn(IBM)
rBP <- dailyReturn(BP)
rGOOG <- dailyReturn(GOOG)
#We put all data into df to use in multivariate model
rX <- data.frame(rIBM,rBP,rGOOG)
names(rX) [1] <-"rIBM"
names(rX) [2] <-"rBP"
names(rX) [3] <-"rGOOG"

#Univariate GARCH model
#Model specifications
ug_spec=ugarchspec()
ug_spec
#change specification
ug_spec <- ugarchspec(mean.model = list(armaOrder=c(1,0)))
ug_spec

#model estimation
ugfit = ugarchfit(spec =ug_spec, data = rIBM)
ugfit

#Ugfit has two sloys one called @fit and the other @model
ugfit@fit
#Checking for all the elemeents in the fit drawer and model drawer
names(ugfit@fit)
names(ugfit@model)
#print the estimated coefficients
ugfit@fit$coef
#Variances
ug_var <- ugfit@fit$var  #conditional variances
ug_res2 <- (ugfit@fit$residuals)^2   #Estimated squared residuals
ug_var
ug_res2
#plotting the squared residuals 
plot (ug_res2, type ="l")
lines(ug_var, col="green")

#model forecasting 
#Forecasting conditional variance
ugfore <- ugarchforecast(ugfit, n.head = 10)
ugfore

#plot sigma forecast
ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type ="l")

# Multivariate GARCH models
#model Setup
#dcc models
uspec.n = multispec(replicate(3, ugarchspec(mean.model = list(armaOrder =c(1,0)))))
#Estimate unuvariate GARCH model
multf = multifit(uspec.n, rX)
multf

#specifications for correlation
spec1 = dccspec(uspec=uspec.n, dccOrder = c(1,1), distribution = "mvnorm")
fit1 = dccfit(spec1, data=rX, fit.control = list(eval.se=TRUE), fit = multf)
#Estimated covariances
cov1 = rcov(fit1) #extracts the covariance matrix
cor1= rcor(fit1)#extracts correlation matrix
fit1

#Look at the dimensionsthe number of days and period measured
dim(cor1)

#Look at the last day
cor1[,,dim(cor1)[3]]
#This matrics change daily in dcc models
cor_BG <-cor1 [2,1,] #leaving the last dimension empty
cor_BG <- as.xts(cor_BG) #imposes the xtgs time series format

#And now we plot this
plot (cor_BG)

#Correlation btn the three assets 
par(mfrow = c(3,1))
plot(as.xts(cor1[1,2,]), main = "IBM and BP")
plot(as.xts(cor1[1,3,]), main = "IBM and Google")
plot(as.xts(cor1[2,3,]), main = "BP and Google")

#Forecasts 
dccf1 <- dccforecast(fit1, n.head =10)
dccf1
