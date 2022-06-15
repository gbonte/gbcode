rm(list=ls())
library(gbcode)

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
data(NN5)

nseries<-NCOL(NN5)
H=20
N=NROW(NN5)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL
NMSE6=NULL

method1="lazydirect"
method2="liniter"
method3="lazyiter"
method4="stat_comb"
method5="rnn"
method6="mimo.comb"
n=24
maxfs=5
for (i in 2:nseries){
  TS=scale(remna(NN5[,i]))
  for (Ntr in round(seq (500,N-H,length.out=3))){
    
    TStr=TS[1:Ntr]
    TSts=TS[(Ntr+1):(Ntr+H)]
    Yn=multiplestepAhead(TStr,n=n, H=H,method="stat_naive")
    
    Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1,maxfs=maxfs)
    NMSE=c(NMSE,mean((TSts-Y.cont)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2,maxfs=maxfs)
    NMSE2=c(NMSE2,mean((TSts-Y.cont2)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3,maxfs=maxfs)
    NMSE3=c(NMSE3,mean((TSts-Y.cont3)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4,maxfs=maxfs)
    NMSE4=c(NMSE4,mean((TSts-Y.cont4)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont5=multiplestepAhead(TStr,n=n, H=H,method=method5,maxfs=maxfs)
    NMSE5=c(NMSE5,mean((TSts-Y.cont5)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont6=multiplestepAhead(TStr,n=n, H=H,method=method6,maxfs=maxfs,Kmin=5,C=4)
    lambda=0
    Y.cont6=lambda*(Yn)+(1-lambda)*Y.cont6
    NMSE6=c(NMSE6,mean((TSts-Y.cont6)^2)/(mean((TSts-Yn)^2)))
    
    ## Forecasting error is normalised wrt error of the naive method
    
    cat("series",i,"/", nseries,  "\n", method1, " NMSE=",mean(NMSE),
        method2," NMSE2=", mean(NMSE2), ":",
        method3, " NMSE3=", mean(NMSE3),": \n",
        method4, " NMSE4=", mean(NMSE4),":",
        method5, " NMSE=", mean(NMSE5),":",
        method6, " NMSE=", mean(NMSE6),"\n")
  }
}