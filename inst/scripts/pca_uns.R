## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
N=500

w=rnorm(N,sd=3)
Ig=which(w<0)
Ir=which(w>0)
X1=rnorm(N,sd=2)

X2=X1+w

Xtilde=scale(cbind(X1,X2))

S=svd(Xtilde)
V1=array(S$v[,1],c(2,1))

Z=Xtilde%*%V1

Dc=Z%*%t(V1)

plot(Xtilde[Ig,1],Xtilde[Ig,2],xlab="x1",ylab="x2",col="green",
     cex=.5,xlim=c(-3,3),ylim=c(-3,3))
points(Xtilde[Ir,1],Xtilde[Ir,2],xlab="x1",ylab="x2",col="red",cex=.5)
lines(X1,V1[2]/V1[1]*X1,col="black",lwd=2)
points(Dc[Ig,1],Dc[Ig,2]-0.05,col="green",lwd=3,pch=20)
points(Dc[Ir,1],Dc[Ir,2]+0.05,col="red",pch=20)