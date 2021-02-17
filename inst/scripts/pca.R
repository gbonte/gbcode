rm(list=ls())
N=500

X1=rnorm(N,sd=2)

X2=rnorm(1)*X1+rnorm(N,sd=3)

Xtilde=scale(cbind(X1,X2))

S=svd(Xtilde)
V1=array(S$v[,1],c(2,1))

Z=Xtilde%*%V1

Dc=Z%*%t(V1)

plot(Xtilde[,1],Xtilde[,2],xlab="x1",ylab="x2")
lines(X1,V1[2]/V1[1]*X1,col="red",lwd=2)
points(Dc[,1],Dc[,2],col="red")