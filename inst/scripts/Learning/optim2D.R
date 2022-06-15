rm(list=ls())

J<-function(a1,a2){
  return(a1^2+a2^2-2*a1-2*a2+6)
}

Jprime<-function(a){
  a1=a[1]
  a2=a[2]
  return(c(2*a1-2, 2*a2-2))
}

alpha1=seq(-2,4,by=0.1)
alpha2=seq(-2,4,by=0.1)
z <- outer(alpha1, alpha2, FUN="J")

persp(alpha1,alpha2,z)
contour(alpha1,alpha2,z)

a=c(3,3) ## alpha0 initialization
points(a[1],a[2],lwd=3,col="red")

mu=0.1
for (r in 1:100){
  a=a-mu*Jprime(a)
  points(a[1],a[2],lwd=3,col="red")
  Sys.sleep(1)
}