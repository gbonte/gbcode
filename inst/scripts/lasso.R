## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

library(quadprog)

## min_x 1/2 x^T D x-d^Tx subject to A^Tx>=b

N=25
n=15

LAM=seq(7,0.01,by=-0.05)
Remp=NULL
X=array(rnorm(N*n),c(N,n))
Y=3*X[,1]+X[,2]-X[,n]+rnorm(N,sd=0.1)



lassols<-function(X,Y,lambda=0){
  eps=1e-10
  Y=Y-mean(Y)
  n=NCOL(X)
  Dmat=t(X)%*%X
  DDmat=rbind(Dmat,-Dmat)
  DDmat=cbind(DDmat,-DDmat)+eps*diag(2*n)
  dvec=t(X)%*%Y
  ddvec=rbind(dvec,-dvec)
  AAmat=numeric(2*n)-1
  AAmat=rbind(AAmat,diag(2*n))
  bbvec=c(-lambda,numeric(2*n))
  S=solve.QP(DDmat,ddvec,t(AAmat),bvec=bbvec)
  betahat=S$solution[1:n]-S$solution[(n+1):(2*n)]
  
  return(betahat)
}


lassopred<-function(Xtr,Ytr,Xts,lambda){
  sXtr=scale(Xtr)
  sXts=scale(Xts,attr(sXtr,"scaled:center"),attr(sXtr,"scaled:scale"))
  mYtr=mean(Ytr)
  
  betahat=lassols(sXtr,Ytr-mYtr,lambda)
  
  Yhats=mYtr+sXts%*%betahat
  Yhatr=mYtr+sXtr%*%betahat
  list(Yhatr=Yhatr,Yhats=Yhats, betahat=betahat)
  
}

L=lassopred(X,Y,X,100)
betahat=L$betahat
par(mfrow=c(1,3))
plot(betahat[1],betahat[2],
     xlim=c(-4,4),ylim=c(-4,4),col="red",lwd=5,xlab="b1",ylab="b2")

BETA=NULL
for (lambda in LAM){
  #betahat=lassols(X,Y,lambda)
  #print(betahat)
  L=lassopred(X,Y,X,lambda)
  betahat=L$betahat
  BETA=rbind(BETA,betahat)
  points(betahat[1],betahat[2])
  Remp=c(Remp,mean((Y-L$Yhatr)^2))
  cat(".")
}


plot(LAM,Remp,ylab="Empirical risk",type="l",xlab="L")

plot(LAM,BETA[,1],ylim=c(-5,5),xlab="L",type="l",ylab="Estimation")

for (i in 2:n)
  lines(LAM,BETA[,i])

