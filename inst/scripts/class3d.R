rm(list=ls())
library(rgl)
while (rgl.cur() > 0) { rgl.close() }

example=3
spherical=TRUE
# 1: green gaussian
# 2: green mixture of 2 gaussians
# 3 : green mixture of 4 gaussians
N=1000
BOUND=4
x <- seq(-BOUND, BOUND, by= .1)
y <- x
z<-array(0,dim=c(length(x),length(y)))
#th : rotation angle of the first principal axis
#ax1: length principal axis 1
#ax2: length principal axis 2

p1=0.5
# a priori probability



### Covariance of red distribution
ax1<-1
th=1
ax2<-2
if (spherical)
  ax2<-ax1

Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
A<-array(c(ax1, 0, 0, ax2),dim=c(2,2))
Sigma<-(Rot%*%A)%*%t(Rot)


for (i in 1:length(x)){
  for (j in 1:length(y)){
    z[i,j]<-dmvnorm(c(x[i],y[j]),sigma=Sigma)
  }
}
z[is.na(z)] <- 1

redp<-z

Dred=rmvnorm(N,sigma=Sigma)

### Covariance of green distribution

ax1<-0.2
th=2
ax2<-1
if (spherical)
  ax2<-ax1
Rot<-array(c(cos(th), -sin(th), sin(th), cos(th)),dim=c(2,2)); #rotation matrix
A<-array(c(ax1, 0, 0, ax2),dim=c(2,2))
Sigma2<-(Rot%*%A)%*%t(Rot)

for (i in 1:length(x)){
  for (j in 1:length(y)){
    
    if (example==1){
      z[i,j]<-dmvnorm(c(x[i],y[j]),mean=c(2,2),sigma=Sigma2)
      Dgreen=rmvnorm(N,sigma=Sigma)
    }
    if (example==2){
      z[i,j]<-0.5*dmvnorm(c(x[i],y[j]),mean=c(2,2),sigma=Sigma2)+
        0.5*dmvnorm(c(x[i],y[j]),mean=-c(2,2),sigma=Sigma2)
      Dgreen=rbind(rmvnorm(N/2,mean=c(2,2),sigma=Sigma2),
               rmvnorm(N/2,mean=-c(2,2),sigma=Sigma2))
    }
    if (example==3){
      z[i,j]<-0.25*dmvnorm(c(x[i],y[j]),mean=c(2,2),sigma=Sigma2)+
        0.25*dmvnorm(c(x[i],y[j]),mean=-c(2,2),sigma=Sigma2)+
        0.25*dmvnorm(c(x[i],y[j]),mean=c(2,-2),sigma=Sigma2)+
        0.25*dmvnorm(c(x[i],y[j]),mean=c(-2,2),sigma=Sigma2)
      Dgreen=rbind(rmvnorm(N/4,mean=c(2,2),sigma=Sigma2),
               rmvnorm(N/4,mean=-c(2,2),sigma=Sigma2),
               rmvnorm(N/4,mean=c(2,-2),sigma=Sigma2),
               rmvnorm(N/4,mean=c(-2,2),sigma=Sigma2))
    }
  }
}
z[is.na(z)] <- 1

greenp<-z



#### PLOT
open3d()
bg3d("white")
material3d(col = "black")
persp3d(x, y, redp,  col = "red",main="Class conditional densities", zlab="")



persp3d(x, y, greenp,  col = "green",add=TRUE)

postp=(redp*p1)/(redp*p1+greenp*(1-p1))

open3d()
bg3d("white")
material3d(col = "black")
persp3d(x, y, postp,  col = "red",
        main="Posteriori probability class red", zlab="")


open3d()
bg3d("white")
material3d(col = "black")
persp3d(x, y, 1-postp,  col = "green",main="Posteriori probability class green", zlab="")


open3d()
bg3d("white")
material3d(col = "black")
persp3d(x, y, postp*(1-postp),  col = "yellow",main="Posteriori variance", zlab="")

plot(Dred[,1],Dred[,2],col="red",xlim=c(-BOUND,BOUND),ylim=c(-BOUND,BOUND))
points(Dgreen[,1],Dgreen[,2],col="green")
