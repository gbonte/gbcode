## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
N<-1

## only observation is an interval
interval<-c(-2,1)

MU<-seq(-3,3,by=.01)

Lik<-NULL
for (mu in MU){
  Lik=c(Lik,pnorm(interval[2],mu)-pnorm(interval[1],mu))
  ## pnorm(interval[2],mu)-pnorm(interval[1],mu)= integral gaussian density in interval
}



muhat<-MU[which.max(Lik)]
plot(MU, Lik,main=paste("ML estimate=",muhat),xlab="theta",ylab="Likelihood")