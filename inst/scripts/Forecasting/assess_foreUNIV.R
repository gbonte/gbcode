## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
library(keras)

library(astsa)
library(xts);

visualize=FALSE


NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL
NMSE6=NULL
NMSE7=NULL
NMSE8=NULL
NMSE9=NULL
NMSE10=NULL

n=24
method1="stat_comb"
method2="arima"
method3="mimolin"
method4="lazydirect"
method5="mimocca"
method6="mimo"
method7="lazyiter"
method8="mldirect"
method9="mlmimo"
method10="stat_naive"

colors=c("red","green","magenta","cyan","orange","blue",
         "yellow","grey","aquamarine","brown")

for (r in 1:50){
  rawTS= genfreq(F=1+r,N=sample(100:200,1),sdw=0.25)
  TS=scale(remNA((rawTS)))
  N=length(rawTS)
  for (H in pmin(c(5,10,20,50),round(N/4)))  
    for (i in seq(round(2*N/3),N-H,length.out=20)){
      TStr=TS[1:i]
      Ntr=length(TStr)
      S=detectSeason(TStr,Ls=i+H,pmin=0.01,forced=TRUE, maxs=100)
      TStr=TStr-S$spattern[1:i]-S$strend[1:i]
      SeasP<-S$spattern+S$strend
      
      TSts=TS[(i+1):(i+H)]
      SPts=SeasP[(i+1):(i+H)]
      
      Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1,epochs=15, 
                               nunits=20)+SPts
      NMSE=c(NMSE,mean(TSts-Y.cont)^2)
      Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2,Kmin=3,C=2)+SPts
      NMSE2=c(NMSE2,mean(TSts-Y.cont2)^2)
      Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3,Kmin=3,C=2)+SPts
      NMSE3=c(NMSE3,mean(TSts-Y.cont3)^2)
      Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4,detrend=0,Kmin=3,C=2)+SPts
      NMSE4=c(NMSE4,mean(TSts-Y.cont4)^2)
      Y.cont5=multiplestepAhead(TStr,n=n, H=H,method=method5,detrend=0,Kmin=3,C=2)+SPts
      NMSE5=c(NMSE5,mean(TSts-Y.cont5)^2)
      
      Y.cont6=multiplestepAhead(TStr,n=n, H=H,method=method6,detrend=0,Kmin=3,C=2)+SPts
      NMSE6=c(NMSE6,mean(TSts-Y.cont6)^2)
      
      Y.cont7=multiplestepAhead(TStr,n=n, H=H,method=method7,detrend=0,Kmin=3,C=2)+SPts
      NMSE7=c(NMSE7,mean(TSts-Y.cont7)^2)
      
      Y.cont8=multiplestepAhead(TStr,n=n, H=H,method=method8,detrend=0,Kmin=3,C=2,learner="py.lasso")+SPts
      NMSE8=c(NMSE8,mean(TSts-Y.cont8)^2)
      
      Y.cont9=multiplestepAhead(TStr,n=n, H=H,method=method9,detrend=0,Kmin=3,C=2,learner="py.lasso")+SPts
      NMSE9=c(NMSE9,mean(TSts-Y.cont9)^2)
      
      Y.cont10=multiplestepAhead(TStr,n=n, H=H,method=method10,detrend=0,Kmin=3,C=2)+SPts
      NMSE10=c(NMSE10,mean(TSts-Y.cont10)^2)
      
      allN=cbind(NMSE,NMSE2,NMSE3,NMSE4,NMSE5,NMSE6,NMSE7,NMSE8,NMSE9,NMSE10)
      colnames(allN)<-c(method1,method2,method3,method4,method5,
                        method6,method7,method8,method9,method10)
      sN=sort(apply(allN,2,mean),decr=FALSE,index=TRUE)$ix
      cat("r=",r,"H=", H,"\n")
      for (i in 1:length(sN))
        cat(colnames(allN)[sN[i]], "NMSE=",mean(allN[,sN[i]]),"\n")
      
      Nvis=round(Ntr*2/3)
      if (visualize){
        plot(c(TS[Nvis:Ntr],TSts),type="l",ylab="TS")
        lines(c(TS[Nvis:Ntr]*NA,Y.cont),col=colors[1])
        lines(c(TS[Nvis:Ntr]*NA,Y.cont2),col=colors[2],lwd=3)
        lines(c(TS[Nvis:Ntr]*NA,Y.cont3),col=colors[3])
        lines(c(TS[Nvis:Ntr]*NA,Y.cont4),col=colors[4],lwd=2)
        lines(c(TS[Nvis:Ntr]*NA,Y.cont5),col=colors[5],lwd=3)
        lines(c(TS[Nvis:Ntr]*NA,Y.cont6),col=colors[6],lwd=3)
        lines(c(TS[Nvis:Ntr]*NA,Y.cont7),col=colors[7],lwd=3)
        lines(c(TS[Nvis:Ntr]*NA,Y.cont8),col=colors[8],lwd=3)
        lines(c(TS[Nvis:Ntr]*NA,Y.cont9),col=colors[9],lwd=3)
        lines(c(TS[Nvis:Ntr]*NA,Y.cont10),col=colors[10],lwd=3)
        
        legend("topleft",c(method1,method2,method3,method4,method5,
                           method6,method7,method8,method9,method10),
               col=colors,lty=1,cex=0.5)
        
      }
      
    }
}