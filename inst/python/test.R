rm(list=ls())

library(gbcode)
library(reticulate)
R=read.csv("SP500.csv")

X=R[,-1]

cat("N=",NROW(X)," n= ", NCOL(X),"\n")
N=NROW(X)
dates=colnames(X)
Itr=1:2000
Its=(max(Itr)+1):N

aNMSE=NULL
mout=480
mlearn1="py.rf_regr"
mlearn2="py.gb_regr"
for (nin in 100:(mout-1)){
  Xtr=X[Itr,1:nin]
  Ytr=X[Itr,mout]-X[Itr,nin]
  Xts=X[Its,1:nin]
  Yts=X[Its,mout]-X[Its,nin]
  

  fs=mrmr(Xtr,Ytr,nmax=50)
  
  N=NROW(Xtr)
  n=NCOL(Xtr[,fs])
  Nts=NROW(Xts[,fs])
  m=NCOL(Ytr)
  pyX<<-Xtr[,fs];   pyXts<<-Xts[,fs];   pyY<<-Ytr;   pyN<<-N;   pyn<<-n;   pyNts<<-Nts;  pym<<-m;
  plearn<<-"gb_regr"
  py_run_file("libpy.py")
  
  NMSE=mean((Yts-Yhat)^2)/var(Yts)
  Yhat2=pred(algo=mlearn2,Xtr[,fs],array(Ytr,c(length(Ytr),1)),Xts[,fs],class=FALSE)
  NMSE2=mean((Yts-Yhat2)^2)/var(Yts)
  
  aNMSE=c(aNMSE,NMSE)
  cat("nin=",nin,"predicting ",dates[mout], "at ",dates[nin],   
      " NMSE=", NMSE," NMSE2=", NMSE2,  "\n")
}

