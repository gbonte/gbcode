## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
library(reticulate)

library(readr)
energy_set_api <- read_csv("data/energy_set_api.csv")

rawTS=cbind(energy_set_api[,"value"])


assess=TRUE
visualize=TRUE

plot(rawTS[,1],ylab="TS")
N=length(rawTS)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL
NMSE6=NULL
TS=scale(remNA((rawTS)))
n=12
method1="mimo.comb"
method2="lazyiter"
method3="lazydirect"
method4="darts_nbeats"
method5="darts_tft"
method6="mimo"
colors=c("red","green","magenta","cyan","orange","yellow")
if (assess)
  for (H in c(72))
    for (i in seq(NROW(TS)-(24*100),NROW(TS)-H,by=24)){
    
      TStr=TS[max(1,i-(24*100)):i]
      Ntr=length(TStr)
      TSts=TS[(i+1):(i+H)]
      
      Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1)
      NMSE=c(NMSE,mean((TSts-Y.cont)^2))
      Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2)
      NMSE2=c(NMSE2,mean((TSts-Y.cont2)^2))
      Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3)
      NMSE3=c(NMSE3,mean((TSts-Y.cont3)^2))
      Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4,nepochs=5, 
                                nunits=10)
      NMSE4=c(NMSE4,mean((TSts-Y.cont4)^2))
      Y.cont5=multiplestepAhead(TStr,n=n, H=H,method=method5,nepochs=5, 
                                nunits=10)
      NMSE5=c(NMSE5,mean((TSts-Y.cont5)^2))
      Y.cont6=multiplestepAhead(TStr,n=n, H=H,method=method6)
      NMSE6=c(NMSE6,mean((TSts-Y.cont6)^2))
      
      cat("H=", H, "i=",i, ":", method1, "NMSE=",mean(NMSE), method2, " NMSE2=", mean(NMSE2),
          method3, " NMSE3=", mean(NMSE3),
          method4, " NMSE4=", mean(NMSE4),
          method5, " NMSE5=", mean(NMSE5),
          method6, " NMSE6=", mean(NMSE6),"\n")
      Nvis=round(Ntr-(2*H))
      if (visualize){
        plot(c(TStr[Nvis:Ntr],TSts),type="l",ylab="TS",lwd=5,col="black")
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont),col=colors[1])
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont2),col=colors[2],lwd=3)
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont3),col=colors[3])
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont4),col=colors[4],lwd=2)
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont5),col=colors[5],lwd=3)
        lines(c(TStr[Nvis:Ntr]*NA,Y.cont6),col=colors[6],lwd=3)
        legend("topleft",c(method1,method2,method3,method4,method5,method6),
               col=colors,lty=1,cex=0.5)
        
      }
      
    }
