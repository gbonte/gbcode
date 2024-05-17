rm(list=ls())
## Taylor approximation of f(x)=sin(x)+cos(x)
par(mfrow=c(1,1))

k=25
X=seq(-15,15,length=150)
Y=numeric(length(X))
for (i in 0:k){
  Y=Y+(-1)^i*(X^(2*i))/factorial(2*i)+
    (-1)^i*(X^(2*i+1))/factorial(2*i+1)
  plot(X,sin(X)+cos(X),col="red",lwd=3,type="l",
       main=paste("Taylor approximation k=",i))
  lines(X,Y,lwd=5)
  
  browser()
  
}


  