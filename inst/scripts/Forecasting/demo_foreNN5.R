rm(list=ls())

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
H=50
N=NROW(NN5)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
for (i in 2:nseries){
  TS=scale(remna(NN5[,i]))
  TStr=TS[1:(N-H)]
  TSts=TS[(N-H+1):N]
  
  Y.cont=multiplestepAhead(TStr,n=20, H=H,method="lazydirect")
  NMSE=c(NMSE,mean(TSts-Y.cont)^2)
  Y.cont2=multiplestepAhead(TStr,n=20, H=H,method="stat_theta")
  NMSE2=c(NMSE2,mean(TSts-Y.cont2)^2)
  Y.cont3=multiplestepAhead(TStr,n=20, H=H,method="mimo")
  NMSE3=c(NMSE3,mean(TSts-Y.cont3)^2)
  
  cat("NMSE=",mean(NMSE), " NMSE2=", mean(NMSE2)," NMSE3=", mean(NMSE3),"\n")
  
}