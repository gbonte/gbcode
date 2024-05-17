rm(list=ls())
par(bg = "white")
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
     xlim=c(-XL,XL),ylim=c(-XL,XL),xlab="x1",ylab="x2")
for (i in 2:4)
  points(X[which(CC==i),1],X[which(CC==i),2],col=cols[i])
points(1,0,col="black",lwd=5)
points(0,1,col="black",lwd=5)
#####################@
A=cbind(c(-1,-1),c(-1,2))

Y=t(A %*% t(X))
plot(Y[which(CC==1),1],Y[which(CC==1),2],col=cols[1],
     xlim=c(-XL,XL),ylim=c(-XL,XL),xlab="y1",ylab="y2")
for (i in 2:4)
  points(Y[which(CC==i),1],Y[which(CC==i),2],col=cols[i])
points(A[1,1],A[2,1],col="black",lwd=5)
points(A[1,2],A[2,2],col="black",lwd=5)
########################
library(rgl)
A=cbind(c(1,-1,2.5),c(0.9,2,2))

Z=t(A %*% t(X))

plot3d(Z[which(CC==1),1],Z[which(CC==1),2],Z[which(CC==1),3],col=cols[1],
       xlab="z1",ylab="z2",zlab="z3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL))
for (i in 2:4)
  points3d(Z[which(CC==i),1],Z[which(CC==i),2],Z[which(CC==i),3],col=cols[i])

points3d(A[1,1],A[2,1],A[3,1],col="black",size=15) # first column of A
points3d(A[1,2],A[2,2],A[3,2],col="black",size=15)