## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
set.seed(0)
library(gbcode)
N=50


# correlated inputs
X1=rnorm(N)
X2=rnorm(N)
X3=rnorm(N)
X4=rnorm(N)

W=rnorm(N,sd=0.25)


## number of irrilevant features
irril=50
X=scale(cbind(X1,X2,X3,X4,array(rnorm(N*irril),c(N,irril))))

Y=scale(X1^2+2*X2+X3+X4)+W

n=NCOL(X)

LAM=seq(1,100,by=5)
E<-array(NA,c(N,length(LAM)))
## LEAVE-ONE-OUT loop
for (i in 1:N){
  
  Xtr=cbind(numeric(N-1)+1,X[-i,])
  Ytr=Y[-i]

  Xts=c(1,X[i,])
  
  cnt=1
  for (lam in LAM){
    betahat=solve(t(Xtr)%*%Xtr+lam*diag(n+1))%*%t(Xtr)%*%Ytr
    Yhati=Xts%*%betahat
    E[i,cnt]=(Y[i]-Yhati)^2
    cnt=cnt+1
  }
}


cat("MSEloo =",apply(E,2,mean),"\n")

lambest=LAM[which.min(apply(E,2,mean))]

XX=cbind(numeric(N)+1,X)
betahat=solve(t(XX)%*%XX+lambest*diag(n+1))%*%t(XX)%*%Y

print(sort(abs(betahat),decr=TRUE,index=TRUE)$ix[1:4]-1)