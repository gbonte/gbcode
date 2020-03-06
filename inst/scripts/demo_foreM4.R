rm(list=ls())
load("data/TSERIES.Rdata")
visual=FALSE
method1="stat_comb"
method2="stat_comb"
aNMSE=NULL
aNMSE2=NULL
for (i in seq(50001,100000,by=100)){
  
  TS=scale(TSERIES[[i]]$ts)
  N<-length(TS)
  H=TSERIES[[i]]$H
  
  for (Ntr in seq(max(round(2*N/3),N-10*H),(N-H),5)){
    
    TS.tr=TS[1:Ntr]
    TS.ts=TS[(Ntr+1):(Ntr+H)]
    modtrnd=lm(TS.tr ~ seq(TS.tr))
    FF=0.25
    stp=round(FF*Ntr) ## starting point
    sItr=stp:Ntr
    sTS.tr=TS[sItr]
    #trnd=pred("lin",sItr,sTS.tr,sItr,classi=FALSE) #modtrnd$fit
    #trnd.ts=pred("lin",sItr,sTS.tr,seq(stp,Ntr+H),classi=FALSE)
    #trnd.ts=trnd.ts[(length(trnd.ts)-H+1):length(trnd.ts)]
    
    trnd=NULL
    h=2
    for (s in (stp-h):(Ntr-h))
      trnd=c(trnd,multiplestepAhead(TS[1:s],n=3, H=h,method=method2)[h])
    
  
    TTS.tr=sTS.tr-trnd
    Y.cont2=multiplestepAhead(TS.tr,n=2, H=H,method=method2)
    Y.cont=multiplestepAhead(TTS.tr,n=1, H=H,method=method1,Kmin=5,C=5)+Y.cont2
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
    cat("i=",i, "Nt=",Ntr, " aNMSE=",mean(aNMSE), "aNMSE2=", mean(aNMSE2),"\n")
  }
  cat("i=",i, " aNMSE=",mean(aNMSE), "aNMSE2=", mean(aNMSE2),"\n")
  
}