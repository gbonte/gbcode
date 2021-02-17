## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


library(mvtnorm)

n=4
rho=0.2
SigmaD=runif(4,1,2)
Corr=array(rho,c(n,n))
diag(Corr)=1

Sigma=sqrt(diag(SigmaD))%*%Corr%*%sqrt(diag(SigmaD))

Corr2=diag(1/sqrt(diag(Sigma)))%*%Sigma%*%diag(1/sqrt(diag(Sigma)))

N=100000
D=rmvnorm(N,sigma=Sigma)

for (i in 1:(n-1))
  for (j in (i+1):n)
  print(cor(D[,i],D[,j]))