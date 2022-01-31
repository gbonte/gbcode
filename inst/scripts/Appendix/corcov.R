## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi
rm(list=ls())

library(mvtnorm)

n=4
rho=0.72 ## bivariate correlation correlation 
SigmaD=runif(4,1,2) ## diagonal of covariance matrix: marginal variances
Corr=array(rho,c(n,n))
diag(Corr)=1

Sigma=diag(sqrt(SigmaD))%*%Corr%*%diag(sqrt(SigmaD))

Corr2=diag(1/sqrt(diag(Sigma)))%*%Sigma%*%diag(1/sqrt(diag(Sigma)))

N=100000
D=rmvnorm(N,sigma=Sigma)

for (i in 1:(n-1))
  for (j in (i+1):n)
  cat(Corr2[i,j],":",cor(D[,i],D[,j]),"\n")