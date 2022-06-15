rm(list=ls())
source("inst/scripts/dopler.R")
library(RSNNS)



rbftrain<-function(X,Y,Xts,K,sigma=1){
  
  N=length(X)
  Nts=length(Xts)
  centers=seq(min(X),max(X),length.out=K)
  W=array(NA,c(N,K))
  
  for (i in 1:N)
    for (k in 1:K)
      W[i,k]=exp(-(X[i]-centers[k])^2)
  d<-data.frame(Y,W)
  colnames(d)[1]<-'Y'
  mod<-lm(Y~.,data=d)
  
  Wts=array(NA,c(Nts,K))
  
  for (i in 1:Nts)
    for (k in 1:K)
      Wts[i,k]=exp(-(Xts[i]-centers[k])^2)
  
  dts<-data.frame(Wts)
  colnames(dts)[1]<-colnames(d)[2]
  
  return(predict(mod,newdata=dts))
} 

N<-200
X<-sort(rnorm(2*N))
Y<-sin(2*X)+rnorm(2*N,sd=0.1)
Itr=seq(1,2*N,by=2)
Its=setdiff(1:(2*N),Itr)
Xtr=X[Itr]
Ytr=Y[Itr]
Xts=X[Its]
Yts=Y[Its]


for (number.neurons in c(3,5,10)){
  
 
  p<-rbftrain(X,Y,Xts,K=number.neurons)
  
  plot(Xts,Yts, main=paste("Number operating regions=",number.neurons))
  lines(Xts,p,col="red")
  
}
