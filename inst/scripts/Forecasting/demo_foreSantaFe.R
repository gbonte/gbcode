## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
library(keras)

remna<-function(ts){
  N=length(ts)
  wna<-which(is.na(ts))
  if (length(wna)>1){
    for (w in wna){
      tsinf=ts[max(setdiff(1:(w-1),wna))]
      tssup=ts[min(setdiff((w+1):N,wna))]
      ts[w]=mean(c(tsinf,tssup),na.rm=TRUE)
    }
  }
  return(ts)
  
}
##data(lynx)
data(A)
rawTS=c(A[1:5000,1])
#rawTS=c(lynx)

assess=TRUE
visualize=TRUE

plot(rawTS,ylab="TS")
N=length(rawTS)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL
TS=scale(remna((rawTS)))
n=12
method1="clazydirect"
method2="mimo.acf.lin"
method3="lazydirect"
method4="lazyiter"
method5="mimo.comb"
colors=c("red","green","magenta","cyan","orange")
if (assess)
  for (H in c(50,100,200,400))
    for (i in 1000){
      TStr=TS[1:i]
      TSts=TS[(i+1):(i+H)]
      
      Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1,Kmin=3,C=2)
      NMSE=mean(TSts-Y.cont)^2
      Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2,Kmin=3,C=2)
      NMSE2=mean(TSts-Y.cont2)^2
      Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3,Kmin=3,C=2)
      NMSE3=mean(TSts-Y.cont3)^2
      Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4,detrend=0,Kmin=3,C=2,
                                nunits=50,epochs=50)
      NMSE4=mean(TSts-Y.cont4)^2
      Y.cont5=multiplestepAhead(TStr,n=n, H=H,method=method5,detrend=0,Kmin=3,C=2)
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
