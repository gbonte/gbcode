#
## Based on https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/
rm(list=ls())
library(gbcode)
library(tidyverse)
setwd(paste(find.package("gbcode"),"scripts/Forecasting",sep="/"))

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)
library(recipes)
library(rsample)
library(keras)


sun_spots <- datasets::sunspot.month %>%
  tk_tbl() %>%
  mutate(index = as_date(index)) %>%
  as_tbl_time(index = index)

H<-20

periods_train <- 20 * 30
periods_test  <- H
skip_span     <- 12

rolling_origin_resamples <- rolling_origin(
  sun_spots,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

detrend=0
method1="lstm"
method2="mimo.acf"
method3="lazydirect"
method4="mimo.comb"
colors=c("red","green","magenta","cyan","orange","yellow")
L=length(rolling_origin_resamples$splits)

n=12
aLSTM2=NULL
aLL1=NULL
aLL2=NULL
aLL3=NULL
aLL4=NULL
for (S in round(L/2):L){
  split    <- rolling_origin_resamples$splits[[S]]
  df_trn <- training(split)
  df_tst <- testing(split)
  TStrain=df_trn$value
  TStest=df_tst$value
  H=length(TStest)
  mu=mean(TStrain)
  stdTS=sd(TStrain)
  nTStrain=(TStrain-mu)/stdTS
  Ntr=length(nTStrain)
 
  Yhat1=multiplestepAhead(nTStrain,n,
                         H=H,method = method1,nunits=50, 
                         epochs = 500,detrend=detrend)*stdTS+mu
 
  Yhat2=multiplestepAhead(nTStrain,n,
                         H=H,method = method2,Kmin=3,C=3,detrend=detrend)*stdTS+mu
  Yhat3=multiplestepAhead(nTStrain,n,
                         H=H,method = method3,Kmin=3,C=3,detrend=detrend)*stdTS+mu
  
  Yhat4=multiplestepAhead(nTStrain,n,
                          H=H,method = method4,Kmin=3,C=3,detrend=detrend)*stdTS+mu
  
  
  aLL1=c(aLL1,mean(abs(TStest-Yhat1)))
  aLL2=c(aLL2,mean(abs(TStest-Yhat2)))
  aLL3=c(aLL3,mean(abs(TStest-Yhat3)))
  aLL4=c(aLL4,mean(abs(TStest-Yhat4)))
  cat("Split=",S, "length(TS)=", length(TStrain), 
      " H=",H,"| \n", "MSE: ", method1, "=",mean(aLL1), 
      method2, "=",mean(aLL2),
      method3, "=",mean(aLL3), method4, "=",mean(aLL4) ,"\n")
  Nvis=round((2*Ntr/3))
  plot(c(TStrain[Nvis:Ntr],TStest),ylab="TS")
  points(c(NA*TStrain[Nvis:Ntr],TStest),lwd=2)
  lines(c(NA*TStrain[Nvis:Ntr],Yhat1),col=colors[1])
  lines(c(NA*TStrain[Nvis:Ntr],Yhat2),col=colors[2])
  lines(c(NA*TStrain[Nvis:Ntr],Yhat3),col=colors[3])
  lines(c(NA*TStrain[Nvis:Ntr],Yhat3),col=colors[4])
  legend("topleft",c(method1,method2,method3,method4),
         col=colors,lty=1,cex=0.5)
  browser()
}