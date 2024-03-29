## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



# s_gaussXYZ.R
# Script: visualizes different bivariate gaussians with different
# orientation and axis' length
rm(list=ls())
library(mvtnorm)
x <- seq(-10, 10, by= .5)
y <- x

z<-array(0,dim=c(length(x),length(y)))

#th : rotation angle of the first principal axis
#ax1: length principal axis 1
#ax2: length principal axis 2

ax1<-1

for (th in seq(0,pi,by=pi/8)){
  for (ax2 in c(1,2,4,8,16)){
    
    Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
    A<-array(c(ax1, 0, 0, ax2),dim=c(2,2))
    Sigma<-(Rot%*%A)%*%t(Rot)
    E<-eigen(Sigma)
    print(paste("Eigenvalue of the Variance matrix=",E$values))
    print(Sigma)
    for (i in 1:length(x)){
      for (j in 1:length(y)){
        z[i,j]<-dmvnorm(c(x[i],y[j]),sigma=Sigma)
      }
    }
    z[is.na(z)] <- 1
    
    
    op <- par(bg = "white")
    prob.z<-z
    
    persp(x, y, prob.z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
    #           contour(x,y,z)
    title (paste("BIVARIATE; Rotation=",round(th,digits=3),
                 "; Axis 1=", ax1, "; Axis 2=", ax2))
    readline(prompt="Press [enter] to continue")
  }
}

