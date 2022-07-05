## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
library(keras)
library(fpp2)

data(AirPassengers)
rawTS=c(AirPassengers)
data(ausbeer)
#rawTS=c(ausbeer)
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
par(mfrow=c(1,1))
n=12

method1="stat_comb"
method2="mimo"
method3="rnn"
method4="lindirect"
method5="mimo.comb"
colors=c("red","green","magenta","cyan","orange")
TS=scale(remNA(rawTS))

X=TS


N=length(TS)
if (assess)
  for (H in c(40))
    for (i in seq(round(2*N/3),N-H,by=1)){
      TStr=TS[1:i]
      TSts=TS[(i+1):(i+H)]
      
      Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1,detrend=detrend)
      NMSE=mean(TSts-Y.cont)^2
      Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2,Kmin=5,C=5,detrend=detrend)
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
        plot(c(TStr,TSts),type="l", ylim=range(TSts)+c(-1,1),ylab="TS")
        lines(c(TStr*NA,Y.cont),col=colors[1])
        lines(c(TStr*NA,Y.cont2),col=colors[2],lwd=3)
        lines(c(TStr*NA,Y.cont3),col=colors[3])
        lines(c(TStr*NA,Y.cont4),col=colors[4],lwd=2)
        lines(c(TStr*NA,Y.cont5),col=colors[5],lwd=3)
        legend("topleft",c(method1,method2,method3,method4,method5),
               col=colors,lty=1,cex=0.5)
        browser()
      }
      
    }
