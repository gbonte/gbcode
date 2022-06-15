## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
data(TSERIES)
visual=FALSE
method1="stat_comb"
method2="stat_comb"
aNMSE=NULL
aNMSE2=NULL
for (i in seq(50001,100000,by=100)){
  
  TS=scale(TSERIES[[i]]$ts)
  N<-length(TS)
  H=TSERIES[[i]]$H
  
  for (Ntr in round(seq(2*N/3,(N-H),length.out=5))){
    
    TS.tr=TS[1:Ntr]
    TS.ts=TS[(Ntr+1):(Ntr+H)]

    
    Y.cont=multiplestepAhead(TS.tr,n=2, H=H,method=method1,Kmin=5,C=5)
    Y.cont2=multiplestepAhead(TS.tr,n=1, H=H,method=method2)
    Y.n=multiplestepAhead(TS.tr,n=1, H=H,method="stat_naive",Kmin=5,C=5)
    E=(TS.ts-Y.cont)
    E2=(TS.ts-Y.cont2)
    En=(TS.ts-Y.n)
    NMSE=mean(E^2)/mean(En^2)
    NMSE2=mean(E2^2)/mean(En^2)
    if (visual){
      plot(c(TS.tr,TS.ts),type="l",ylim=c(4/3*min(TS),4/3*max(TS)))
      title(paste(method1, "NMSE=", round(NMSE,2),":", method2, " NMSE2=", round(NMSE2,2),sep= " "))
      NAtr=numeric(Ntr)+NA
      lines(c(NAtr,Y.cont),col="green")
      lines(c(NAtr, Y.cont2),col="red")
      browser()
    }
    aNMSE=c(aNMSE,NMSE)
    aNMSE2=c(aNMSE2,NMSE2)
    cat("i=",i, "Nt=",Ntr, method1," aNMSE=",mean(aNMSE), method2, "aNMSE2=", mean(aNMSE2),"\n")
  }
  cat("i=",i, method1, " aNMSE=",mean(aNMSE), method2, "aNMSE2=", mean(aNMSE2),"\n")
  
}