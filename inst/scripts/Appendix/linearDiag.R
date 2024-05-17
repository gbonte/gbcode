
## based on the Example 4.12 Book "Mathematics for machine learning" Deisenroth
rm(list=ls())
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
open3d()
plot3d(X[which(CC==1),1],X[which(CC==1),2],X[which(CC==1),3],col=cols[1],
       xlab="x1",ylab="x2",zlab="x3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL),
       main="X space")
for (i in 2:8)
  points3d(X[which(CC==i),1],X[which(CC==i),2],X[which(CC==i),3],col=cols[i])



#############
A=cbind(c(1,-0.8,1),c(-0.8,1,0),c(1,0,2))
SVD=svd(A)
Z=t(A %*% t(X))
open3d()
plot3d(Z[which(CC==1),1],Z[which(CC==1),2],Z[which(CC==1),3],col=cols[1],
       xlab="z1",ylab="z2",zlab="z3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL),
       main="Single step")
for (i in 2:8)
  points3d(Z[which(CC==i),1],Z[which(CC==i),2],Z[which(CC==i),3],col=cols[i])

points3d(A[1,1],A[2,1],A[3,1],col="black",size=15) # first column of A
points3d(A[1,2],A[2,2],A[3,2],col="black",size=15)
points3d(A[1,3],A[2,3],A[3,3],col="black",size=15)



#####################@
A1=t(SVD$v)

Y=t(A1 %*% t(X))
open3d()
plot3d(Y[which(CC==1),1],Y[which(CC==1),2],Y[which(CC==1),3],col=cols[1],
       xlab="d1",ylab="d2",zlab="d3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL),
       main="D step")
for (i in 2:8)
  points3d(Y[which(CC==i),1],Y[which(CC==i),2],Y[which(CC==i),3],col=cols[i])


########################

A2=rbind(diag(SVD$d))

D=t(A2 %*% t(Y))
open3d()
plot3d(D[which(CC==1),1],D[which(CC==1),2],D[which(CC==1),3],col=cols[1],
       xlab="d1",ylab="d2",zlab="d3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL),
       main="D step")
for (i in 2:8)
  points3d(D[which(CC==i),1],D[which(CC==i),2],D[which(CC==i),3],col=cols[i])


########################

A3=SVD$u 

Z=t(A3 %*% t(D))
open3d()
plot3d(Z[which(CC==1),1],Z[which(CC==1),2],Z[which(CC==1),3],col=cols[1],
       xlab="u1",ylab="u2",zlab="u3",xlim=c(-XL,XL),ylim=c(-XL,XL),zlim=c(-XL,XL),
       main="U step")
for (i in 2:8)
  points3d(Z[which(CC==i),1],Z[which(CC==i),2],Z[which(CC==i),3],col=cols[i])

