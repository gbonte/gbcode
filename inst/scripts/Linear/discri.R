## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
norm<-function(x){
  sqrt(sum(x^2))
}

N1<-100
N2<-100
P<-c(N1,N2)/(N1+N2)
sigma2<-1

mu.1 <- c(-1,-2)

mu.2<-c(2,5)


X1<-cbind(rnorm(N1,mu.1[1],sqrt(sigma2)),rnorm(N1,mu.1[2],sqrt(sigma2)))

X2<-cbind(rnorm(N2,mu.2[1],sqrt(sigma2)),rnorm(N2,mu.2[2],sqrt(sigma2)))


plot(X1[,1],X1[,2],col="red",xlim=c(-10,10),ylim=c(-10,10),xlab="x1",ylab="x2")
points(X2[,1],X2[,2],col="green")

Xd<--10:10

w<-mu.1-mu.2
x0<-0.5*(mu.1+mu.2)-sigma2/(norm(mu.1-mu.2)^2)*(mu.1-mu.2)*log(P[1]/P[2])

m<--w[1]/w[2]
intc<-w[1]/w[2]*x0[1]+x0[2]

abline(a=intc,b=m)
## lines(x=c(mu.1[1],mu.2[1]),y=c(mu.1[2],mu.2[2]))
points(x0[1],x0[2])
