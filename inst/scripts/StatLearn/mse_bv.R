## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi
# mse_bv.R
# Script: checks the equality MSE=B^2+V




N<-10
mu<-0
sdev<-10
R<-10000

I<-seq(-50,50,by=0.5)
p<-dnorm(I,mean=mu,sd=sdev)
plot(I,p,type="l",
     main=paste("Distribution of  r.v. z: var=",sdev^2))

mu.hat<-array(0,dim=c(R,1))
for (r in 1:R){
  
  D<-rnorm(N,mean=mu,sd=sdev)
  
  mu.hat[r,1]<-mean(D)
}

err<-mu.hat-mu

MSE<-mean(err^2)
BIAS<-mean(mu.hat)-mu
VARIANCE<-var(mu.hat)

print(paste("MSE=",MSE))
print(paste("BIAS^2+VARIANCE=",BIAS^2+VARIANCE))

