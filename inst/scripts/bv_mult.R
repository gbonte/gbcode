## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


library(MASS)
par(ask=TRUE)

n<-4 # number input variables
p<-n+1
N<-100 # number training data
X<-array(runif(N*n,min=-20,max=20),c(N,n))
X<-cbind(array(1,c(N,1)),X)

beta<-sample(1:10,p) ## test with different values

R<-10000
sd.w<-5


beta.hat<-array(0,c(p,R))
var.hat.w<-numeric(R)
Y.hat<-array(NA,c(R,N))
for (r in 1:R){
  Y<-X%*%beta+rnorm(N,sd=sd.w)
  
  
  beta.hat[,r]<-ginv(t(X)%*%X)%*%t(X)%*%Y
  
  
  Y.hat[r,]<-X%*%beta.hat[,r]
  e<-Y-Y.hat[r,]
  var.hat.w[r]<-(t(e)%*%e)/(N-p)
}

hist(var.hat.w,main=paste("Distribution of var.hat.w: var w=", sd.w^2))
for (i in 1:p){
  hist(beta.hat[i,], main=paste("Distribution of beta.hat.",i,": beta",i,"=", beta[i]))
}



for (i in 1:N){
  # test unbiasedness prediction
  cat("i=",i,"E[yhat_i]=",mean(Y.hat[,i]), " f(x_i)=",X[i,]%*%beta,"\n")
  
  # comparison analytical and simulated variance of the prediction
  cat("i=",i,"prediction variance=",sd.w^2*(t(X[i,])%*%ginv(t(X)%*%X)%*%X[i,]),
      "MC value=",var(Y.hat[,i]),"\n \n")
  
}

