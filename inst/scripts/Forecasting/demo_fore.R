## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
library(fpp2)
remna<-function(ts){
  N=length(ts)
  wna<-which(is.na(ts))
  if (length(wna)>1){
    for (w in wna){
      tsinf=ts[max(setdiff(1:(w-1),wna))]
      tssup=ts[min(setdiff((w+1):N,wna))]
      ts[w]=mean(c(tsinf,tssup))
    }
  }
  return(ts)
  
}
##data(lynx)
rawTS=c(lynx)

data("sunspot.month")
rawTS=c(sunspot.month)

data(euretail)
data(h02)
rawTS=c(lynx)
assess=TRUE
require(forecast)
#rawTS=c(AirPassengers)

#rawTS=arima.sim(n=10000,list(order=c(1,0,1),ar=0.7,ma=0.5))
plot(rawTS)
N=length(rawTS)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL
TS=scale(remna((rawTS)))
n=5
if (assess)
for (H in c(5,10,20,40))
  for (i in seq(round(N/2),(N-H),by=round(N/20))){
    TStr=TS[1:i]
    TSts=TS[(i+1):(i+H)]
    
    Y.cont=multiplestepAhead(TStr,n=n, H=H,method="lazydirect",detrend=0)
    NMSE=c(NMSE,mean(TSts-Y.cont)^2)
    Y.cont2=multiplestepAhead(TStr,n=n, H=H,method="stat_theta",detrend=0)
    NMSE2=c(NMSE2,mean(TSts-Y.cont2)^2)
    Y.cont3=multiplestepAhead(TStr,n=n, H=H,method="lindirect",detrend=0)
    NMSE3=c(NMSE3,mean(TSts-Y.cont3)^2)
    Y.cont4=multiplestepAhead(TStr,n=n, H=H,method="arima",detrend=0)
    NMSE4=c(NMSE4,mean(TSts-Y.cont4)^2)
    
    Y.cont5=NULL
    for (r in 1:20){
      Y.cont5=cbind(Y.cont5,multiplestepAhead(TStr+rnorm(length(TStr),sd=runif(1,0.01,0.5)),
                                              n=n, H=H,method="lazydirect",detrend=0))
    }
    Y.conts5=apply(Y.cont5,1,mean)
    NMSE5=c(NMSE5,mean(TSts-Y.cont5)^2)
    cat("H=", H, "NMSE=",mean(NMSE), " NMSE2=", mean(NMSE2)," NMSE3=", mean(NMSE3),
        " NMSE4=", mean(NMSE4)," NMSE5=", mean(NMSE5),"\n")
  }

i=round(2*N/3)
H=min(80,N-i)
TStr=TS[1:i]
TSts=TS[(i+1):(i+H)]

Y.cont=multiplestepAhead(TStr,n=n, H=H,method="arima",detrend=0)

plot(TS,type="l")
lines((i+1):(i+H),Y.cont,col="red")

