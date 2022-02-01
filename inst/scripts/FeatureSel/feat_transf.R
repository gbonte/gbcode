## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



# x^+y^2=1
rm(list=ls())
sdw=0.1
N1<-100
r1<-1
x1<-runif(N1,-r1,r1)
y1up=sqrt((r1^2-x1^2))+rnorm(N1,sd=sdw)
y1down=-sqrt((r1^2-x1^2))+rnorm(N1,sd=sdw)

N2<-200
r2<-2
x2<-runif(N2,-r2,r2)
y2up=sqrt((r2^2-x2^2))+rnorm(N2,sd=sdw)
y2down=-sqrt((r2^2-x2^2))+rnorm(N2,sd=sdw)

par(mfrow=c(1,2))
plot(x1,y1up,col="red",
     ylim=c(-3,3),xlim=c(-r2,r2),ylab="x2",xlab="x1", main="Original space")
points(x1,y1down,col="red")

points(x2,y2up,col="green")
points(x2,y2down,col="green")


plot(x1^2,y1up^2,col="red",
     ylim=c(0,r2^2),xlim=c(-1,r2^2),ylab="x2^2",xlab="x1^2", main="Transformed space")
points(x1^2,y1down^2,col="red")

points(x2^2,y2up^2,col="green")
points(x2^2,y2down^2,col="green")
