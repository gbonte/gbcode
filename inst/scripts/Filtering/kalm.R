# x1(k+1)=x1(k)+u1(k)+w1
# x2(k+1)=x2(k)+u2(k)+w2

rm(list=ls())
library(mvtnorm)
A=array(c(0.9, 0.1,0.1,0.9),c(2,2))
B=array(1,c(2,2))

H=diag(2)

Q=diag(2)
R=0.1*diag(2)


Pplus=1e6*diag(2);

x=array(c(1,10),c(2,1))
xplus=array(0,c(2,1))
xmin=NULL
z=x
T=100
par(mfrow=c(1,1))
I2=diag(2)
for (k in 2:T){
   
   u=runif(2,-1,1)
   ## u(k-1)
   
   x=cbind(x,A%*%array(x[,k-1],c(2,1))+B%*%u+t(rmvnorm(1,array(0,c(2,1)),Q)))
   ## Dynamic system state update
   
   z=cbind(z,H%*%array(x[,k-1],c(2,1))+t(rmvnorm(1,array(0,c(2,1)),R)))      
   ## Dynamic system observations
   
   
   xmin= A%*%array(xplus[,k-1],c(2,1))+B%*%u
   ## KF step 1
   
   Pmin=A%*%Pplus%*%t(A)+Q
   ## KF step 2
   
   K=Pmin%*%t(H)%*%solve(H%*%Pmin%*%t(H)+I2)
   ## KF step 3
   
   xplus=cbind(xplus,xmin+K%*%(z[,k]-H%*%xmin))
   ## KF step 4
   
   Pplus=(I2-K%*%H)%*%Pmin;
   ## KF step 5
   
   
   if (FALSE){
      ## step by step tracking visualisation
      plot(x[1,],x[2,],type="l",main="Tracking",xlab="x1",ylab="x2")
      points(xplus[1,],xplus[2,],col="red")
      points(x[1,tt],x[2,tt],col="black",lwd=4)
      points(xplus[1,tt],xplus[2,tt],col="red",lwd=4)
      legend('topleft',c('x2','estimated by KF'),lty=c(1,1),col=c("black","red"))
      readline()
   }
}


par(mfrow=c(1,3))
plot(1:T,x[1,],type="l",xlab="k",ylab="x1")
points(1:T,xplus[1,],col="red")
legend('topleft',c('x1','estimated by KF'),lty=c(1,1),col=c("black","red"))

plot(1:T,x[2,],type="l",xlab="k",ylab="x2")
points(1:T,xplus[2,],col="red")
legend('topleft',c('x2','estimated by KF'),lty=c(1,1),col=c("black","red"))


plot(x[1,],xplus[1,],type="l",xlab="x1",ylab="x2")
points(x[2,],xplus[2,],col="red")

MAE=mean(abs(x-xplus))  ## error after filtering
MAE2=mean(abs(x-z))  ## error without filtering

