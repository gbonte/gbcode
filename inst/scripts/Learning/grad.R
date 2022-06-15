## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
set.seed(0)
par(mfrow=c(1,1))
N=100

X=rnorm(N)
Y=X^3+rnorm(N,sd=0.1)

## Least squares solution
XX=cbind(numeric(N)+1,X)
alphaLS=solve(t(XX)%*%XX)%*%t(XX)%*%Y

Yhat=alphaLS[1]+alphaLS[2]*X
e=Y-Yhat
cat("min LS error=",mean(e^2),"\n")
plot(X,Y)
lines(X,Yhat,lwd=5)

cat("\n --- \n")

## Random search

I=1000
bestJ=Inf
for (i in 1:I){
  alpha=rnorm(2,sd=2)
  Yhat=alpha[1]+alpha[2]*X
 
  e=Y-Yhat
  
  if (mean(e^2)<bestJ){
    bestalpha=alpha
    bestJ=mean(e^2)
    cat("random search: error at iteration ", i, "=",mean(e^2),"\n")
  }
}
cat("\n --- \n")

### Gradient based search

I=50
alpha=numeric(2)
mu=0.001
for (i in 1:I){
  Yhat=alpha[1]+alpha[2]*X
  lines(X,Yhat,col="green")
  e=Y-Yhat
  alpha[1]=alpha[1]+mu*2*sum(e)
  alpha[2]=alpha[2]+mu*2*sum(e*X)
  if (i%%10==0)
    cat("gradient-based: error at iteration ", i, "=",mean(e^2),"\n")
}

cat("\n --- \n")


### Levenberg Marquardt

I=3
alpha=numeric(2)

for (i in 1:I){
  Yhat=alpha[1]+alpha[2]*X
  lines(X,Yhat,col="red")
  e=Y-Yhat
  cat("Levenberg Marquardt: error at iteration ", i, "=",mean(e^2),"\n")
  E=-cbind(2, 2*X)
  bestJ=Inf
  for (lambda in seq(0.01,10,by=0.01)){
    alphat=alpha-solve(t(E)%*%E+lambda*diag(2))%*%t(2*E)%*%e
    #alphat=alpha-lambda*t(E)%*%e
    Yhatt=alphat[1]+alphat[2]*X
    et=Y-Yhatt
    
    if (mean(et^2)<bestJ){
      bestlambda=lambda
      bestJ=mean(et^2)
      
    }
   
  }
  alpha=alpha-solve(t(E)%*%E+bestlambda*diag(2))%*%t(2*E)%*%e
  #alpha=alpha-bestlambda*t(E)%*%e
 
}

