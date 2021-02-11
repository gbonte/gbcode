## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
norm<-function(x){
  sqrt(sum(x^2))
}

Lik<-function(X,Y,alpha0,alpha1,alpha2){
  N<-length(Y)
  L=0
  for (i in 1:N)
    L=L+log(1+exp(-Y[i]*(alpha0+alpha1*X[i,1]+alpha2*X[i,2])))
  
  return (L)
}


Grad<-function(X,Y,alpha0,alpha1,alpha2){
  N<-length(Y)
  G0=0
  G1=0
  G2=0
  for (i in 1:N){
    G1=G1-X[i,1]*Y[i]
    G2=G2-X[i,2]*Y[i]
    G0=-Y[i]
  }
   
  
  return(c(G0,G1,G2))
}


N1<-50
N2<-50
P<-c(N1,N2)/(N1+N2)
sigma2<-1

mu.1 <- rnorm(2)

mu.2<-rnorm(2)


X1<-cbind(rnorm(N1,mu.1[1],sqrt(sigma2)),rnorm(N1,mu.1[2],sqrt(sigma2)))

X2<-cbind(rnorm(N2,mu.2[1],sqrt(sigma2)),rnorm(N2,mu.2[2],sqrt(sigma2)))

X=rbind(X1,X2)
Y=numeric(N1+N2)-1
Y[1:N1]=1


A0=seq(-2,2,by=0.05)
A1=seq(-2,2,by=0.05)
A2=seq(-2,2,by=0.05)

alpha0=rnorm(1)
alpha1=rnorm(1)
alpha2=rnorm(1)
mu=0.01
plot(X1[,1],X1[,2],col="red",xlim=c(-10,10),ylim=c(-10,10),xlab="x1",ylab="x2")
points(X2[,1],X2[,2],col="green")

for ( r in 1:2000){
  G=Grad(X,Y,alpha0,alpha1,alpha2)
  alpha0=alpha0-mu*G[1]
  alpha1=alpha1-mu*G[2]
  alpha2=alpha2-mu*G[3]
  if (r%%200==0)
  lines(X[,1],-alpha1/alpha2*X[,1]-alpha0/alpha2,col="blue")
}






bestL=Inf
for (alpha0 in  A0)
  for (alpha1 in A1)
  for (alpha2 in A2){
    L=Lik(X,Y,alpha0,alpha1,alpha2)
    
    if (L<bestL){
      bestL=L
      bestalpha0=alpha0
      bestalpha1=alpha1
      bestalpha2=alpha2
      
    }
    
    
  }



lines(X[,1],-bestalpha1/bestalpha2*X[,1]-bestalpha0/bestalpha2)



