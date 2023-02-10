## regression example
rm(list=ls())
library(randomForest)
NMSE=NULL
n=10
N=300
X=array(rnorm(N*n),c(N,n))
Y=cbind(X[,1]*X[,2]+rnorm(N,sd=0.1),X[,n-1]*X[,n]+rnorm(N,sd=0.1),X[,n-1]+log(abs(X[,n]))+rnorm(N,sd=0.1))
m=NCOL(Y)
Itr=sample(N,round(N/2))
Its=setdiff(1:N,Itr)
Xtr=X[Itr,]
Ytr=Y[Itr,]
Xts=X[Its,]
Yts=Y[Its,]
if (m==1){
  algos=c("lin","lazy","py.rf_regr","py.lasso_regr","py.keras_regr")
}else {
  algos=c("py.ridge_regr","py.rf_regr","py.piperf_regr","py.lasso_regr","py.keras_regr","py.pls_regr",
          "py.enet_regr","py.gb_regr","py.knn_regr","py.pipeknn_regr","py.ab_regr")
}
for (a in 1:length(algos)){
  Yhat=pred(algos[a],Xtr,Ytr,Xts, classi=FALSE)
  e=Yts-Yhat
  ## normalized mean squared error
  if (m==1)
    NMSE=c(NMSE,mean(e^2)/var(Yts))
  else {
    NMSEj=NULL
    for (j in 1:m)
      NMSEj=c(NMSEj,mean(e[,j]^2)/var(Yts[,j]))
    NMSE=c(NMSE,mean(NMSEj))
  }
}
names(NMSE)=algos
print(sort(NMSE))
