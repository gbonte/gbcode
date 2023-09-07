

## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

## Visualization of leave-one-out strategy with a linear model
rm(list=ls())
par(ask=FALSE)

f<-function(x,ord){
  f<-1
  for (i in 1:ord)
    f<-f+(x^i)
  
  f
}


set.seed(1)

n<-1
N<-15

x<-seq(-2,2,length.out=N)
N<-length(x)
sd.w<-2.5

O<-3
Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
data.tr<-cbind(Y,x)

X=cbind(numeric(N)+1,x)


beta=solve(t(X)%*%X)%*%t(X)%*%Y
Yhat=X%*%beta

for (i in 1:N){
  Xtri<-X[-i,]
  Ytri=Y[-i]
  betai=solve(t(Xtri)%*%Xtri)%*%t(Xtri)%*%Ytri
  Yhati=X%*%betai
  ei=(Y[i]-Yhat[i])^2
  plot(x,Y,main=paste("Squared loo residual=",round(ei,2)))
  points(x[i],Y[i],col="red",cex=2,pch=20)
  lines(x,Yhat,lwd=3)
  lines(x,Yhati,col="red",lwd=3)
  segments(x[i],Y[i],x[i],Yhati[i],col="red",lwd=3,lty=2)
  browser()
}
