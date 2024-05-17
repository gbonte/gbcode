## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

## KL divergence between two n variate Gaussian distributions
rm(list=ls())
library(mvtnorm)
set.seed(2)

n=6 # dimension of the multivariate r.v.s

mu0=cbind(rnorm(n))
mu1=cbind(rnorm(n,mean=0.25,sd=0.5))
 
A0 <- matrix(runif(n^2,-1,1), ncol=n) 
Sigma0 <- t(A0) %*% A0
A1 <- matrix(runif(n^2,-1,1), ncol=n) 
Sigma1 <- t(A1) %*% A1

KL=0.5*(log(det(Sigma0))-log(det(Sigma1))-n+
          t(mu0-mu1)%*%solve(Sigma1)%*%(mu0-mu1)+
          sum(diag(solve(Sigma1)%*%Sigma0)))

KLmc=NULL
R=5000 ## number of MC trials
for (r in 1:R){
  z=rmvnorm(1,mu0,Sigma0)  ## random generation of a sample z ~ p0
  KLmc=c(KLmc,dmvnorm(z,mu0,Sigma0,log=TRUE)-
           dmvnorm(z,mu1,Sigma1,log=TRUE))
}
KLmc[which(is.infinite(KLmc))]=NA
cat("KL=",KL,"MC computation of KL=",mean(KLmc,na.rm=TRUE))
