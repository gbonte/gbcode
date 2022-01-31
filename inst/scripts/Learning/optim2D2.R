rm(list=ls())

J<-function(a1,a2){
  return((a1^4+a2^4)/4-(a1^3+a2^3)/3-a1^2-a2^2+4)
}

Jprime<-function(a){
  a1=a[1]
  a2=a[2]
  return(c(a1^3-a1^2-2*a1,a2^3-a2^2-2*a2))
}

alpha1=seq(-3,4,by=0.1)
alpha2=seq(-3,4,by=0.1)
z <- outer(alpha1, alpha2, FUN="J")

persp(alpha1,alpha2,z)
par( mar=c(4,4,2,2))
contour(alpha1,alpha2,z,levels=c(-1,0,1,2,5,10,20),
        xlab="alpha1",ylab="alpha2")

a=c(3,3) ## alpha0 initialization
points(a[1],a[2],lwd=3,col="red")

mu=0.01
for (r in 1:100){
  a=a-mu*Jprime(a)
  points(a[1],a[2],lwd=3,col="red")
  Sys.sleep(1)
}