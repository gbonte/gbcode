rm(list=ls())

J<-function(a){
  return(a^4/4-a^3/3-a^2+2)
}

Jprime<-function(a){
  return(a^3-a^2-2*a)
}

alpha=seq(-2,4,by=0.1)
plot(alpha,J(alpha),type="l")


a=3 ## alpha0 initialization
points(a,J(a),lwd=3,col="red")

mu=0.01
for (r in 1:100){
  a=a-mu*Jprime(a)
  points(a,J(a),lwd=3,col="red")
  Sys.sleep(1)
}