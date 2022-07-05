## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
library(keras)
library(fpp2)

data(h02)
rawTS=c(h02)
rawTS=diff(rawTS)
assess=TRUE
visualize=TRUE
detrend=0.01
plot(rawTS,ylab="TS",type="l")
N=length(rawTS)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL

n=24
frequency=c(12)
method1="stat_comb"
method2="lstm"
method3="lazydirect"
method4="rnn"
method5="mimo.comb"
colors=c("red","green","magenta","cyan","orange")

TS=remNA(rawTS)

TS=scale(TS)
N=length(TS)
if (assess)
  for (H in c(50))
    for (i in seq(round(2*N/3),N-H,by=1)){
      TStr=TS[1:i]
      TSts=TS[(i+1):(i+H)]
      
      Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1,detrend=detrend)
      NMSE=mean(TSts-Y.cont)^2
      Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2,detrend=detrend)
      NMSE2=mean(TSts-Y.cont2)^2
      Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3,detrend=detrend)
      NMSE3=mean(TSts-Y.cont3)^2
      Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4,
                                nunits=50,epochs=50,detrend=detrend)
      NMSE4=mean(TSts-Y.cont4)^2
      Y.cont5=multiplestepAhead(TStr,n=n, H=H,method=method5,detrend=detrend)
      NMSE5=mean(TSts-Y.cont5)^2
      
      cat("H=", H, method1, "NMSE=",mean(NMSE), method2, " NMSE2=", mean(NMSE2),
          method3, " NMSE3=", mean(NMSE3),
          method4, " NMSE4=", mean(NMSE4),
          method5, " NMSE5=", mean(NMSE5),"\n")
      if (visualize){
        plot(TSts,type="l")
        lines(Y.cont,col=colors[1])
        lines(Y.cont2,col=colors[2],lwd=3)
        lines(Y.cont3,col=colors[3])
        lines(Y.cont4,col=colors[4],lwd=2)
        lines(Y.cont5,col=colors[5],lwd=3)
        legend("topleft",c(method1,method2,method3,method4,method5),
               col=colors,lty=1,cex=0.5)
        browser()
      }
      
    }
