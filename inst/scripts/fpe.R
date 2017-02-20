
library(MASS)
par(ask=TRUE)
set.seed(0)

n<-3 # number input variables
p<-n+1
p.max<-8
N<-150 # number training data
x<-runif(N,min=-1,max=1)
X<-cbind(array(1,c(N,1)),x,x^2,x^3,x^4,x^5,x^6,x^7)
beta<-c(1,array(seq(1,n),c(n,1)))

sd.w<-1

Y<-X[,1:p]%*%beta+rnorm(N,sd=sd.w)
Yts<-X[,1:p]%*%beta+rnorm(N,sd=sd.w)

R.emp<-NULL
MISE<-NULL
FPE<-NULL
no.par<-NULL


for (i in 2:p.max){
  XX<-X[,1:i]
  invX<-ginv(t(XX)%*%XX)
  beta.hat<-invX%*%t(XX)%*%Y
  Y.hat<-XX%*%beta.hat
  
  no.par<-c(no.par,i)
  
  e<-Y-Y.hat
  R.emp<-c(R.emp,(t(e)%*%e)/N)
  
  
  e.ts<-Yts-Y.hat
  MISE<-c(MISE,(t(e.ts)%*%e.ts)/N)
  FPE<-c(FPE,(1+i/N)/(1-i/N)* (t(e)%*%e)/N)
  
}

plot(no.par-1,R.emp,type="l",
     xlab="# parameters", ylab="Empirical risk",main="Empirical risk",xlim=c(2,7))

plot(no.par-1,MISE,type="l",
     xlab="# parameters", ylab="Generalization error",main="Generalization error",xlim=c(2,7))
plot(no.par-1,FPE,type="l",
     xlab="# parameters", ylab="FPE",main="FPE",xlim=c(2,7))


cat("which.min(R.emp)=",which.min(R.emp),"\n")
cat("which.min(MISE)=",which.min(MISE),"\n")
cat("which.min(FPE)=",which.min(FPE),"\n")

