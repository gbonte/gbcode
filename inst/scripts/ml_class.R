## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
mu.red=1 ## associated to 0
mu.green=-2

N=1000

X=c(rnorm(N/2,mu.green),rnorm(N/2,mu.red))
Y=numeric(N)
Y[1:N/2]=1


TH<-seq(-5,5,by=.01)

Lik<-NULL
for (theta in TH){
  lik=0
  for (i in 1:N){
    Pgreen=1/(1+exp(-(X[i]*theta)))
    Pred=1-Pgreen
    if (i>0)
      lik=lik+log(Pgreen/(Pgreen+Pred))
    else
      lik=lik+log(Pred/(Pgreen+Pred))
  }
    Lik=c(Lik,lik)
}



thetahat<-TH[which.max(Lik)]
plot(TH, Lik,main=paste("ML estimate=",thetahat),xlab="theta",ylab="Likelihood")

xx=seq(-5,5,by=0.1)
Pgreen=1/(1+exp(-(xx*thetahat)))
Pred=1-Pgreen

plot(xx,Pgreen, type="l",col="green",yli=c(0,1) )
lines(xx,Pred, col="red" )

condPgreen=dnorm(xx,mu.green)/(dnorm(xx,mu.green)+dnorm(xx,mu.red))

lines(xx,condPgreen, col="green",lwd=3 )