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
H=100
N=NROW(NN5)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL

method1="lazydirect"
method2="stat_theta"
method3="mimo.acf"
method4="arima"
n=30
for (i in 2:nseries){
  TS=scale(remna(NN5[,i]))
  TStr=TS[1:(N-H)]
  TSts=TS[(N-H+1):N]
  
  Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1)
  NMSE=c(NMSE,mean(TSts-Y.cont)^2)
  
  Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2)
  NMSE2=c(NMSE2,mean(TSts-Y.cont2)^2)
  
  Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3)
  NMSE3=c(NMSE3,mean(TSts-Y.cont3)^2)
  
  Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4)
  NMSE4=c(NMSE4,mean(TSts-Y.cont4)^2)
  
  cat("series",i,"/", nseries,  "\n", method1, " NMSE=",mean(NMSE),
      method2," NMSE2=", mean(NMSE2),
      method3, " NMSE3=", mean(NMSE3),
      method4, " NMSE4=", mean(NMSE4),"\n")
  
}