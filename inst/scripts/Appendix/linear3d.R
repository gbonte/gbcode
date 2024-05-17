## Linear transformation and matrices

rm(list=ls())
library(rgl)
close3d()
par(bg = "white")
x <- seq(-1, 1, length.out = 20)
y <- seq(-1, 1, length.out = 20)
z <- seq(-1, 1, length.out = 20)

cols=c("red","green","blue","yellow","brown","orange","grey","cyan")

X<-as.matrix(expand.grid(x,y,z))

CC <- numeric(NROW(X))
CC[which(X[,1]>0 & X[,2]>0 & X[,3]>0) ]=1
CC[which(X[,1]>0 & X[,2]>0 & X[,3]<0) ]=2
CC[which(X[,1]>0 & X[,2]<0 & X[,3]>0) ]=3
CC[which(X[,1]>0 & X[,2]<0 & X[,3]<0) ]=4
CC[which(X[,1]<0 & X[,2]>0 & X[,3]>0) ]=5
CC[which(X[,1]<0 & X[,2]>0 & X[,3]<0) ]=6
CC[which(X[,1]<0 & X[,2]<0 & X[,3]>0) ]=7
CC[which(X[,1]<0 & X[,2]<0 & X[,3]<0) ]=8

XL=4
par(mfrow=c(1,2))

plot3d(X[which(CC==1),1],X[which(CC==1),2],X[which(CC==1),3],col=cols[1],
       xlab="x1",ylab="x2",zlab="x3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL))
for (i in 2:8)
  points3d(X[which(CC==i),1],X[which(CC==i),2],X[which(CC==i),3],col=cols[i])

points3d(1,0,0,col="black",size=15) # first column of A
points3d(0,1,0,col="black",size=15)
points3d(0,0,1,col="black",size=15)

########################
open3d()
A=cbind(c(1,-1,1.5),c(0.9,0,1.2),c(0,0,1))

Z=t(A %*% t(X))

plot3d(Z[which(CC==1),1],Z[which(CC==1),2],Z[which(CC==1),3],col=cols[1],
       xlab="z1",ylab="z2",zlab="z3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL))
for (i in 2:8)
  points3d(Z[which(CC==i),1],Z[which(CC==i),2],Z[which(CC==i),3],col=cols[i])

points3d(A[1,1],A[2,1],A[3,1],col="black",size=15) # first column of A
points3d(A[1,2],A[2,2],A[3,2],col="black",size=15)
points3d(A[1,3],A[2,3],A[3,3],col="black",size=15)