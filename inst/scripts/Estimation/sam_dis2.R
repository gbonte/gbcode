## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


# script sam_dis2.R
# it visualizes the distribution of the estimator
# of the variance of a gaussian random variable


par(ask=TRUE)
N<-10
mu<-0
sdev<-10
R<-10000

I<-seq(-50,50,by=.5)
p<-dnorm(I,mean=mu,sd=sdev)
plot(I,p,type="l",
     main=paste("Distribution of  r.v. z: var=",sdev^2))

var.hat<-array(0,dim=c(R,1))
std.hat<-array(0,dim=c(R,1))
for (r in 1:R){

  D<-rnorm(N,mean=mu,sd=sdev)
  
  var.hat[r,1]<-var(D)
  std.hat[r,1]<-sd(D)
}

I2<-seq(0,2*sdev^2,by=.5)
hist(var.hat,freq=FALSE, 
     main= paste("Variance estimator on N=",N, " samples: mean=",mean(var.hat))) #,xlim=range(I2))


ch<-(var.hat*(N-1))/(sdev^2)
hist(ch,freq=FALSE)
p.var.hat<-dchisq(I2,df=N-1)
lines(I2,p.var.hat,type="l")


