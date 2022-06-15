## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)



N<-10000

X1<-rnorm(N)
X2<-rnorm(N)

Y=X1+X2

I=which(abs(Y)<0.01)

plot(X1,X2,xlab="x1",ylab="x2",main="COLLIDER (y=x1+x2): conditioned on y=0 ")
points(X1[I],X2[I],col="red")


D=data.frame(X1,X2)
colnames(D)<-c("X1","X2")
reg1 <- lm(X2~X1,data=D) 

abline(reg1,col="black",lw=2)

D2=data.frame(X1[I],X2[I])
colnames(D2)<-c("X1","X2")
reg2 <- lm(X2~X1,data=D2) 

abline(reg2,col="red",lw=2)