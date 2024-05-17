
## based on the Example 4.12 Book "Mathematics for machine learning" Deisenroth
rm(list=ls())
par(bg = "white")
library(rgl)
close3d(dev = cur3d(), silent = TRUE)
x <- seq(-1, 1, length.out = 50)
y <- seq(-1, 1, length.out = 50)

cols=c("red","green","blue","yellow")

X<-as.matrix(expand.grid(x,y))

CC <- numeric(NROW(X))
CC[which(X[,1]>0 & X[,2]>0)]=1
CC[which(X[,1]>0 & X[,2]<0)]=2
CC[which(X[,1]<0 & X[,2]>0)]=3
CC[which(X[,1]<0 & X[,2]<0)]=4

XL=4
par(mfrow=c(1,2))

plot(X[which(CC==1),1],X[which(CC==1),2],col=cols[1],
     xlim=c(-XL,XL),ylim=c(-XL,XL),xlab="x1",ylab="x2",main="Original space")
for (i in 2:4)
  points(X[which(CC==i),1],X[which(CC==i),2],col=cols[i])


A = cbind(c(1, 0, 1), c(-0.8, 1, 0))
SVD=svd(A,nu=3)
Z=t(A %*% t(X))

plot3d(Z[which(CC==1),1],Z[which(CC==1),2],Z[which(CC==1),3],col=cols[1],
       xlab="z1",ylab="z2",zlab="z3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL),
       main="Single step")
for (i in 2:4)
  points3d(Z[which(CC==i),1],Z[which(CC==i),2],Z[which(CC==i),3],col=cols[i])


#####################@
A1=t(SVD$v)

Y=t(A1 %*% t(X))
plot(Y[which(CC==1),1],Y[which(CC==1),2],col=cols[1],
     xlim=c(-XL,XL),ylim=c(-XL,XL),xlab="v1",ylab="v2",main="V step")
for (i in 2:4)
  points(Y[which(CC==i),1],Y[which(CC==i),2],col=cols[i])

########################

A2=rbind(diag(SVD$d),c(0,0))

D=t(A2 %*% t(Y))
open3d()
plot3d(D[which(CC==1),1],D[which(CC==1),2],D[which(CC==1),3],col=cols[1],
       xlab="d1",ylab="d2",zlab="d3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL),
       main="D step")
for (i in 2:4)
  points3d(D[which(CC==i),1],D[which(CC==i),2],D[which(CC==i),3],col=cols[i])


########################

A3=SVD$u 

Z=t(A3 %*% t(D))
open3d()
plot3d(Z[which(CC==1),1],Z[which(CC==1),2],Z[which(CC==1),3],col=cols[1],
       xlab="u1",ylab="u2",zlab="u3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL),
       main="U step")
for (i in 2:4)
  points3d(Z[which(CC==i),1],Z[which(CC==i),2],Z[which(CC==i),3],col=cols[i])

