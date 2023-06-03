#This script contains the 1st practical work of Linear Times Series course of 2nd year
library(zoo)
library(tseries)
library(fUnitRoots)
df=read.csv(file ="C:/Users/tayoy/Documents/ENSAE/2A/S2/Linear Time Series/TD/donnees1.csv", sep = ";")
s=ts(df[[1]][1:(length(df[[1]])-4)])
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
acf(dsns)[1] #-0.305 => clearely suggests there is no unit root

#2 following lines are equivalent : 
adf.test(dsns, k = 1) 
adfTest(dsns, lags = 1, "ct")


#Tests if a ts is white noise
Ljung_Box<-function(s,maxlag=24, df=1){
  indexes = 1:maxlag
  p_val=c()
  for (lag in indexes){
    if (lag<=df) p = NA
    else{
      p = Box.test(s, lag=lag, type = "Ljung-Box", fitdf=df)$p.value
    }
    p_val=c(p_val, p)
  }
  res = cbind(indexes, p_val)
}

#Runs all ADF tests until residuals are tested to be white noise
ADF_finder<-function(s, maxlag = 24, type = "ct"){
  dfs=c("ct"=3,"c"=2, "nc" = 1)
  reject =TRUE
  i = 0 #lag tested start at 0 : random walk + trend
  while(reject==TRUE & i<=maxlag){
    adf = adfTest(s,lags = i, type = type)
    res = adf@test$lm$residuals
    delta_df = dfs[type]
    lb = Ljung_Box(res, maxlag = 24, df = i+delta_df) #nb of df is nb of regressors in ECM fitting
    reject = sum(lb[,2]<0.05,na.rm=TRUE)>0 #we decide to reject hypothesis at 5%
    i=i+1
  }
  if (reject == TRUE)
  {paste("NO WHITE NOISE UNTIL", maxlag, "lags")#never rejected
  }
  else{
    return(list(lags = i-1, adf = adf, p_val = adf@test$p.value))
  }
}

ADF_finder(dsns)
ADF_finder(dsns, type = "c")
ADF_finder(dsns, type = "nc") #cannot reject with a "nc" assumption, but the correct assumption is "c"



