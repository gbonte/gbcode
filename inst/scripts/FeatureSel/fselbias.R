## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



library(MASS)
library(gbcode)
set.seed(0)
n<-4 # number relevant input variables
nirr<-50 # number irrelevant input variables
p<-n+1
N<-20 # number training data
Nts<-5000

X<-array(runif(N*n,min=-2,max=2),c(N,n))
X<-cbind(array(1,c(N,1)),X,array(rnorm(N*nirr),c(N,nirr)))
## the last n variables are irrelevant

Xts<-array(runif(Nts*n,min=-2,max=2),c(Nts,n))
Xts<-cbind(array(1,c(Nts,1)),Xts,array(rnorm(Nts*nirr),c(Nts,nirr)))
beta<-rnorm(p,sd=2) ## test with different values
sd.w<-1

Y<-X[,1:p]%*%beta+rnorm(N,sd=sd.w)
Yts<-Xts[,1:p]%*%beta+rnorm(Nts,sd=sd.w)
set.seed(0)


R<-10000
fset<-NULL
bestr<-NULL
bests<-NULL
for (it in 1:(NCOL(X)-1)){
  MSEf<-numeric(NCOL(X))+Inf
  for (f in setdiff(2:NCOL(X),fset)){
    e=numeric(N)
    for (i in 1:N){
      Yhati=pred("lin",X[-i,c(fset,f)],Y[-i],X[i,c(fset,f)] ,class=FALSE)
      e[i]=Y[i]-Yhati
    }
    MSEf[f]=mean(e^2)

  }## for f
  fset<-c(fset,which.min(MSEf))
  bestr<-c(bestr,min(MSEf))
  
  Yhats=pred("lin",X[,fset],Y,Xts[,fset] ,class=FALSE)
  bests<-c(bests,mean((Yts-Yhats)^2))
  
  print(fset)
  print(bests-bestr)
  plot(bests,ylim=c(0,max(bests)),type="l",main="Selection bias and feature selection",
       col="red",xlab="Feature set size",ylab="")
  lines(bestr,lty=2,lwd=2)
  
  legend(2, 10,c("Test MSE","Internal MSE LOO"),col=c("red","black"),lty=c(1,2),lwd=c(1,2))
} ## for it

