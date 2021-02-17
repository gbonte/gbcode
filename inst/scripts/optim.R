## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

J<-function(a){
  return(a^2-2*a+3)
}


Jprime<-function(a){
  return(2*a-2)
}

alpha=seq(-2,4,by=0.1)

plot(alpha,J(alpha),type="l")


a=-1
mu=0.1
for (r in 1:100){
  a=a-mu*Jprime(a)
  points(a,J(a),lwd=3,col="red")
  Sys.sleep(1)
}