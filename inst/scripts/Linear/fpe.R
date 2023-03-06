## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


library(MASS)
par(ask=TRUE)
set.seed(0)
par(mfrow=c(1,1))
n<-3 # number input variables
p<-n+1
p.max<-25
N<-35 # number training data
x<-sort(runif(N,min=-1,max=1))

X<-array(1,c(N,1))
for (j in 1:p.max)
  X<-cbind(X,x^j)

xts=seq(-1,1,by=0.01)
Xts<-array(1,c(length(xts),1))
for (j in 1:p.max)
  Xts<-cbind(Xts,xts^j)

beta<-c(1,array(seq(1,n),c(n,1)))

sd.w<-0.5

f<-X[,1:p]%*%beta
Y<-f+rnorm(N,sd=sd.w)

fts<-Xts[,1:p]%*%beta
Yts<-fts+rnorm(length(fts),sd=sd.w)

R.emp<-NULL
MISE<-NULL
FPE<-NULL
PSE<-NULL
no.par<-NULL


for (i in 2:min(p.max,N-1)){
  XX<-X[,1:i]
  invX<-ginv(t(XX)%*%XX)
  beta.hat<-invX%*%t(XX)%*%Y
  Y.hat<-XX%*%beta.hat
  
  XXts<-Xts[,1:i]
  Y.hats<-XXts%*%beta.hat
  no.par<-c(no.par,i)
  
  e<-Y-Y.hat
  R.emp<-c(R.emp,(t(e)%*%e)/N)
  sde2hat=1/(N-i)*(t(e)%*%e)
  plot(x,f,type="l",ylim=c(min(Y),max(Y)),lwd=3)
  points(x,Y)
  lines(xts,Y.hats,col="red")
   
  
  e.ts<-Yts-Y.hats
  MISE<-c(MISE,(t(e.ts)%*%e.ts)/N)
  FPE<-c(FPE,(1+i/N)/(1-i/N)* (t(e)%*%e)/N)
  PSE<-c(PSE,(t(e)%*%e)/N+2*sde2hat*i/N)
  title(paste("degree=", i-1, "; MISE_emp=",round(R.emp[i-1],2), "; FPE=",round(FPE[i-1],2),
        "; PSE=",round(PSE[i-1],3)))
  
}

par(mfrow=c(2,2))
plot(no.par-1,R.emp,type="l",
     xlab="# parameters", ylab="Empirical risk",main="Empirical risk",xlim=c(2,10),ylim=c(0,0.4))

plot(no.par-1,MISE,type="l",
     xlab="# parameters", ylab="Generalization error",main="Generalization error",xlim=c(2,10),ylim=c(0,5))
plot(no.par-1,FPE,type="l",
     xlab="# parameters", ylab="FPE",main="FPE",xlim=c(2,10),ylim=c(0,0.5))

plot(no.par-1,PSE,type="l",
     xlab="# parameters", ylab="PSE",main="PSE",xlim=c(2,10),ylim=c(0,0.5))

cat("which.min(R.emp)=",which.min(R.emp),"\n")
cat("which.min(MISE)=",which.min(MISE),"\n")
cat("which.min(FPE)=",which.min(FPE),"\n") 
cat("which.min(PSE)=",which.min(PSE),"\n") 

