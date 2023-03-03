## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

# indep.R
# Script: shows the realization of independent and dependent random
# variables




N<-5000
X<-rnorm(N,sd=5)
X2<-rnorm(N,mean=2,sd=2)
K=1
Y<-K*X+rnorm(N)
Y2<-K*X^2+rnorm(N)
Y3<-K*X2^2+rnorm(N)
Y4<-rnorm(N,sd=0.1)
par(mfrow=c(1,4))
plot(X,Y,ylab="y",
     main=paste("Dep, corr: cor(X,Y)=",round(cor(X,Y),2)))

plot(X,Y2,ylab="y",
     main=paste("Dep, uncorr: cor(X,Y)=",round(cor(X,Y2),2)))

plot(X2,Y3,ylab="y",
     main=paste("Dep, corr: cor(X,Y)=",round(cor(X2,Y3),2)))

plot(X,Y4,ylab="y",
     main=paste("Indep, uncorr: cor(X,Y)=",round(cor(X,Y4),2)),ylim=c(-1,1))

