## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


library(MASS)


n<-4 # number input variables
p<-n+1
N<-20 # number training data

X<-array(runif(N*n,min=-2,max=2),c(N,n))
X<-cbind(array(1,c(N,1)),X,array(rnorm(N*n),c(N,n)))
## the last n variables are irrelevant

set.seed(0)
beta<-0.5*c(1,-1,1,-1,1) ## test with different values

R<-10000
sd.w<-0.5


beta.hat<-array(0,c(p,R))
var.hat.w<-numeric(R)
Y.hat<-array(NA,c(R,N,NCOL(X)))
e.hat<-array(NA,c(R,N,NCOL(X)))
for (r in 1:R){
  for (nfs in 0:(NCOL(X)-1)){
    
    Y<-X[,1:p]%*%beta+rnorm(N,sd=sd.w)
    Xsel<-X[,1:(nfs+1)]
    
    beta.hat<-ginv(t(Xsel)%*%Xsel)%*%t(Xsel)%*%Y
    
    
    Y.hat[r,,nfs+1]<-Xsel%*%beta.hat
    
    
  }
}

aV=NULL
aVmc=NULL
aB=NULL
aMSE=NULL
for (nfs in 0:(NCOL(X)-1)){
  Vh=NULL
  Vmc=NULL
  Bmc=NULL
  MSEmc=NULL
  for (i in 1:N){
    Vh=c(Vh,sd.w^2*(t(X[i,1:nfs])%*%ginv(t(X[,1:nfs])%*%X[,1:nfs])%*%X[i,1:nfs]))  
    Vmc=c(Vmc,var(Y.hat[,i,nfs+1]))
    Bmc=c(Bmc,mean(Y.hat[,i,nfs+1])-X[i,1:p]%*%beta)
    MSEmc=c(MSEmc,mean((Y.hat[,i,nfs+1]-c(X[i,1:p]%*%beta))^2))
  }
    
  # comparison analytical and simulated variance of the prediction
  cat("nfs=",nfs, "variance: th=",mean(Vh),
      "MC =",mean(Vmc),"\n \n", "bias: MC=",mean(Bmc^2),"\n")
  aV=c(aV,mean(Vh))
  aVmc=c(aVmc,mean(Vmc))
  aB=c(aB,mean(Bmc^2))
  aMSE=c(aMSE,mean(MSEmc))
}

plot(0:(NCOL(X)-1),aB,type="l",col="green",xlab="Number of features",
     ylab="",main="Feature selection linear regression: Bias/variance trade-off",
     lty=2)
lines(0:(NCOL(X)-1),aV,col="red",lty=3)
lines(0:(NCOL(X)-1),aVmc,col="red",lty=4)
lines(0:(NCOL(X)-1),aMSE,col="black",lty=1,lwd=2)
legend(5,0.70,c("Bias^2 (MC)","Variance","Variance (MC)", "MSE (MC)"),
       col=c("green","red","red","black"),lty=c(2,3,4,1),lwd=c(1,1,1,2))

