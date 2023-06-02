#This script contains the 1st practical work of Linear Times Series course of 2nd year
library(zoo)
library(tseries)
library(fUnitRoots)
df=read.csv(file ="C:/Users/tayoy/Documents/ENSAE/2A/S2/Linear Time Series/TD/donnees1.csv", sep = ";")
s=ts(df[[1]][1:(t-4)])
t = length(s)
plot(s)
#s seems (periodic + trend)

slag=lag(s,12)
dsns=s-slag
length(dsns)
dev.off()
#layout(matrix(c(1,2)), height = c(2,2) )
plot(s) 
plot(dsns)
kpss.test(dsns, null = "Level")#stationarity is not rejected
acf(dsns)[1] #-3.05 => clearely suggests there is no unit root

#2 following lines are equivalent : 
adf.test(dsns, k = 1) 
adfTest(dsns, lags = 1, "ct")

dfct=adfTest(dsns, lags = 1, "ct") #@test$lm$residuals
dfc=adfTest(dsns, lags = 1, "c")
dfnc=adfTest(dsns, lags = 1, "nc")

     