## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


set.seed(0)
N<-100
sigma.w<-0.5
X<-rnorm(N)
W<-rnorm(N,sd=0.5)

beta0 <- 2
beta1 <- -2
Y<- beta0+beta1*X+W

plot(X,Y)

xhat<-mean(X)
yhat<-mean(Y)

Sxy<- sum((X-xhat)*Y)
Sxx<- sum((X-xhat)*X)

## Univariate least-squares
betahat1<-Sxy/Sxx
betahat0<-yhat-betahat1*xhat

lines(X,betahat0+X*betahat1)

