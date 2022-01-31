## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
norm<-function(x){
  sqrt(sum(x^2))
}

Np<-100
Nn<-100
N=Np+Nn
P<-c(Np,Nn)/(Np+Nn)
sigma2<-3

mu.p <- c(-1,-2)

mu.n<-c(2,5)


Xp<-cbind(rnorm(Np,mu.p[1],sqrt(sigma2)),rnorm(Np,mu.p[2],sqrt(sigma2)))

Xn<-cbind(rnorm(Nn,mu.n[1],sqrt(sigma2)),rnorm(Nn,mu.n[2],sqrt(sigma2)))

X=rbind(Xp,Xn)
Y=numeric(N)-1
Y[1:Np]=1

beta=numeric(3)-2
# b0+b1*x1+b2*x2=0 -> x2=-b0/b2-b1/b2*x1

M=1:N
mu=0.1
while (length(M)>0){
  M=which(Y*(beta[1]+X%*%beta[2:3])<0)
  
  plot(Xp[,1],Xp[,2],col="red",xlim=c(-10,10),ylim=c(-10,10),xlab="x1",ylab="x2",
       main=paste("# misclassified=",length(M)))
  points(Xn[,1],Xn[,2],col="green")
  
  X1=seq(-10,10,by=.1)
  lines(X1,-beta[1]/beta[3]-beta[2]/beta[3]*X1)
  if (length(M)>0){
    beta[1]=beta[1]+mu*sum(Y[M])
    if (length(M)>1)
      beta[2:3]=beta[2:3]+mu*apply(Y[M]*X[M,],2,sum)
    else
      beta[2:3]=beta[2:3]+mu*(Y[M]*X[M,])
  }
}
