## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
library(mvtnorm)
norm<-function(x){
  sqrt(sum(x^2))
}

N=1000

p1=0.4

w1=0.5
w2=0.5

mu11=c(1,1)
mu12=c(-2.1,-1.3)

Sigma1=0.5*diag(2)
N1=N*p1
X=rbind(rmvnorm(round(w1*N1),mu11,Sigma1), 
        rmvnorm(round(w2*N1),mu12,Sigma1))

N0=N-N1
mu0=c(0,0)
Sigma0=0.5*diag(2)
X=rbind(X,rmvnorm(N0,mu0,Sigma0))
Y=numeric(N)
Y[1:N1]=1

Phat1=N1/N
Phat0=N0/N

plot(X[1:N1,1],X[1:N1,2],col="green",xlab="x1",ylab="x2")
points(X[(N1+1):N,1],X[(N1+1):N,2],col="red")

muhat1=apply(X[1:N1,],2,mean)
muhat0=apply(X[(N1+1):N,],2,mean)
Xd<--10:10
sigma2=mean(apply(X,2,var))

w<-muhat1-muhat0
x0<-0.5*(muhat1+muhat0)-sigma2/(norm(muhat1-muhat0)^2)*(muhat1-muhat0)*log(Phat1/Phat0)

m<--w[1]/w[2]
intc<-w[1]/w[2]*x0[1]+x0[2]

abline(a=intc,b=m)
## lines(x=c(mu.1[1],mu.2[1]),y=c(mu.1[2],mu.2[2]))
points(x0[1],x0[2])
