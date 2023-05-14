library(zoo)
library(tseries)
library('fUnitRoots')
filename = "fileTP5.csv"
path = paste("C:/Users/tayoy/Documents/ENSAE/S2/Linear Time Series/TD",filename, sep = "/")
d = read.csv(file = path, sep = ';')

#Q1 File reading
dates_char =d$dates
dates_char[1];dates_char[length(dates_char)]
dates = as.yearmon(seq(from = 1986+3/12, to = 2007+4/12, by = 1/12))

#Q2 Plots
spread = zoo(d$spread, order.by = dates)
dspread = diff(spread,1)
plots = cbind(spread,dspread)
# We can observe that the differenciated series might be stationary,
# while the level series seems to have some deterministic or random trend.

#Q3 Unit roots tests
#both statistics are equivalent but the second object has clearer output
adf1 = adf.test(spread, k=1) #computes the ADF test at lag k with a linear trend and a constant
adf = adfTest(spread, lags = 0, type = "ct") #type = "ct" for a lin trend and cst, "c" for just a cst and "nc" for basic ADF
adf@test$lm #to retrieve the model fitting

#residuals whiteness tests : 
Ljung_Box<-function(s,maxlag=10, df=1){
  indexes = 1:maxlag
  p_val=c()
  for (lag in indexes){
    if (lag<=df) p = NA
    else{
    p = Box.test(s, lag= lag, type = "Ljung-Box", fitdf= df)$p.value
    }
    p_val=c(p_val, p)
  }
  res = cbind(indexes, p_val)
}
#Runs all ADF tests untill  residuals are tested to be white noise
ADF_finder<-function(s, maxlag = 24){
  reject =TRUE
  i = 0 #lag tested start at 0 : random walk + trend
  while(reject==TRUE &i<=maxlag) {
    adf = adfTest(s,lags = i, type = "ct")
    res = adf@test$lm$residuals
    lb = Ljung_Box(res, maxlag = 24, df = i+3) #nb of df is nb of regressors in ECM fitting
    reject = sum(lb[,2]<0.05,na.rm=TRUE)>0 #we decide to reject hypothesis at 5%
    i=i+1
  }
  if (reject == TRUE) {paste("NO WHITE NOISE UNTILL ", maxlag, lags)#never rejected 
  #To be continued 
  }
  
}
ADF_finder(spread)
