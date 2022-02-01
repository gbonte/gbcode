## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
N<-100000

X1<-rnorm(N,sd=0.5)

Y<-X1+rnorm(N,sd=0.2)
X2<-2*Y+rnorm(N,sd=0.2)



I=which(abs(Y)<0.001)

plot(X1,X2,xlab="x1",ylab="x2",main="CHAIN (y=x1+e1, x2=2y+e2): conditioned on y=0")
points(X1[I],X2[I],col="red")


D=data.frame(X1,X2)
colnames(D)<-c("X1","X2")
reg1 <- lm(X2~X1,data=D) 

abline(reg1,col="black",lw=2)

D2=data.frame(X1[I],X2[I])
colnames(D2)<-c("X1","X2")
reg2 <- lm(X2~X1,data=D2) 

abline(reg2,col="red",lw=2)