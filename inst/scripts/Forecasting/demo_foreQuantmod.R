## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
library(keras)

require(quantmod) 
getSymbols("UNRATE",src="FRED") 
chartSeries(UNRATE,theme="white") 
rate <- as.numeric(UNRATE[,1]) 
rawTS <- rate*10 ### change rate to integers


assess=TRUE
visualize=TRUE

plot(rawTS,ylab="TS")
N=length(rawTS)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL
TS=scale(remNA((rawTS)))
n=13
method1="lstm"
method2="mimo.acf.lin"
method3="lazydirect"
method4="lazyiter"
method5="mimo.comb"
colors=c("red","green","magenta","cyan","orange")
if (assess)
  for (H in c(10, 20,30))
    for (i in c(500,600, 700)){
      TStr=TS[1:i]
      Ntr=length(TStr)
      TSts=TS[(i+1):(i+H)]
      
      Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1,epochs=15, 
                               nunits=20)
      NMSE=c(NMSE,mean(TSts-Y.cont)^2)
      Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2,Kmin=3,C=2)
      NMSE2=c(NMSE2,mean(TSts-Y.cont2)^2)
      Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3,Kmin=3,C=2)
      NMSE3=c(NMSE3,mean(TSts-Y.cont3)^2)
      Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4,detrend=0,Kmin=3,C=2)
      NMSE4=c(NMSE4,mean(TSts-Y.cont4)^2)
      Y.cont5=multiplestepAhead(TStr,n=n, H=H,method=method5,detrend=0,Kmin=3,C=2)
      NMSE5=c(NMSE5,mean(TSts-Y.cont5)^2)
      
      cat("H=", H, method1, "NMSE=",mean(NMSE), method2, " NMSE2=", mean(NMSE2),
          method3, " NMSE3=", mean(NMSE3),
          method4, " NMSE4=", mean(NMSE4),
          method5, " NMSE5=", mean(NMSE5),"\n")
      Nvis=round(Ntr*2/3)
      if (visualize){
        plot(c(TStr[Nvis:Ntr],TSts),type="l",ylab="TS")
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont),col=colors[1])
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont2),col=colors[2],lwd=3)
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont3),col=colors[3])
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont4),col=colors[4],lwd=2)
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont5),col=colors[5],lwd=3)
        legend("topleft",c(method1,method2,method3,method4,method5),
               col=colors,lty=1,cex=0.5)
        browser()
      }
      
    }
