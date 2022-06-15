## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
library(fpp2)
require(forecast)
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
data(lynx)
data("sunspot.month")
data(euretail)
data(h02)
data(AirPassengers)

rawTS=c(sunspot.month)
#rawTS=arima.sim(n=10000,list(order=c(1,0,1),ar=0.7,ma=0.5))


assess=TRUE
visualize=TRUE


plot(rawTS)
N=length(rawTS)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL
NMSE6=NULL
TS=scale(remna((rawTS)))
n=5
method1="lazydirect"
method2="stat_theta"
method3="lindirect"
method4="rnn"
method5="arima"
method6="mimo.comb"
colors=c("red","green","magenta","cyan","orange","yellow")
HH<-c(10,20,50)
if (assess)
  for (i in round(seq((N/3),(N-max(HH)),length.out=(5))))
    for (H in HH){
      TStr=TS[1:i]
      TSts=TS[(i+1):min((i+H),N)]
      
      Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1,detrend=0)
      NMSE=c(NMSE,mean(TSts-Y.cont)^2)
      Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2,detrend=0)
      NMSE2=c(NMSE2,mean(TSts-Y.cont2)^2)
      Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3,detrend=0)
      NMSE3=c(NMSE3,mean(TSts-Y.cont3)^2)
      Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4,detrend=0)
      NMSE4=c(NMSE4,mean(TSts-Y.cont4)^2)
      Y.cont5=multiplestepAhead(TStr,n=n, H=H,method=method5,detrend=0)
      NMSE5=c(NMSE5,mean(TSts-Y.cont5)^2)
      Y.cont6=multiplestepAhead(TStr,n=n, H=H,method=method6,detrend=0)
      NMSE6=c(NMSE6,mean(TSts-Y.cont6)^2)
      cat("i=",i, "H=", H, method1, "NMSE=",mean(NMSE), 
          method2, " NMSE2=", mean(NMSE2),
          method3, " NMSE3=", mean(NMSE3),
          method4, " NMSE4=", mean(NMSE4),
          method5, " NMSE5=", mean(NMSE5),
          method6, " NMSE6=", mean(NMSE6),"\n")
      if (visualize){
        plot(TSts,type="l")
        lines(Y.cont,col=colors[1])
        lines(Y.cont2,col=colors[2])
        lines(Y.cont3,col=colors[3])
        lines(Y.cont4,col=colors[4],lwd=2)
        lines(Y.cont5,col=colors[5])
        lines(Y.cont6,col=colors[6])
        legend("topleft",c(method1,method2,method3,method4,method5,method6),
               col=colors,lty=1)
        
      }
    }


