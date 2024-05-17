## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())

library(MASS)
library(gbcode)
set.seed(0)
nrel<-3 # number relevant input variables
nirr<-50 # number irrelevant input variables

N<-10 # number training data
Nval<-500
Nts<-15000

f<-function(X,nrel,sdw){
  N<-NROW(X)
  Y=X[,1]*X[,2]^2+X[,3]+log(abs(X[,nrel]))+apply(abs(X[,1:nrel]),1,sum)+rnorm(N,sd=sdw)
  Y

}

X<-array(runif(N*nrel,min=-2,max=2),c(N,nrel))
X<-cbind(X,array(rnorm(N*nirr),c(N,nirr)))
## the last n variables are irrelevant

Xval<-array(runif(Nval*nrel,min=-2,max=2),c(Nval,nrel))
Xval<-cbind(Xval,array(rnorm(Nval*nirr),c(Nval,nirr)))


Xts<-array(runif(Nts*nrel,min=-2,max=2),c(Nts,nrel))
Xts<-cbind(Xts,array(rnorm(Nts*nirr),c(Nts,nirr)))

sdw<-0.25

Y<-f(X,nrel,sdw)
Yts<-f(Xts,nrel,sdw)
Yval<-f(Xval,nrel,sdw)


fset<-NULL
bestr<-NULL
bests<-NULL
bestval<-NULL
mod="rf"
for (it in 1:(NCOL(X)-1)){
  MSEf<-numeric(NCOL(X))+Inf
  for (f in setdiff(1:NCOL(X),fset)){
    e=numeric(N)
    #for (i in 1:N){
      Yhati=pred(mod,X[,c(fset,f)],Y[],X[,c(fset,f)] ,class=FALSE)
      #e[i]=Y[i]-Yhati
      e=Y-Yhati
    #}
    MSEf[f]=mean(e^2)

  }## for f
  fset<-c(fset,which.min(MSEf))
  bestr<-c(bestr,min(MSEf))
  
  Yhatval=pred(mod,X[,fset],Y,Xval[,fset] ,class=FALSE)
  bestval<-c(bestval,mean((Yval-Yhatval)^2))
  
  
  Yhats=pred(mod,X[,fset],Y,Xts[,fset] ,class=FALSE)
  bests<-c(bests,mean((Yts-Yhats)^2))
  
  print(fset)
  cat("MSE test - MSE tr=",bests-bestr,"\n")
  plot(bests,ylim=c(0,max(bests)),type="l",main="Selection bias and feature selection",
       col="red",xlab="Feature set size",ylab="")
  lines(bestval,lty=1,lwd=2,col="green")
  lines(bestr,lty=2,lwd=2)
  
  legend(2, 10,c("Test MSE","Validation MSE","Internal MSE LOO"),col=c("red","green","black"),lty=c(1,1,2),lwd=c(1,1,2))
} ## for it

