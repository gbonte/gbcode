## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


library(MASS)
par(ask=TRUE)
set.seed(0)

nreal<-3
preal<-nreal+1
n<-10 # number input variables
p<-n+1
p.max<-15
N<-20 # number training data

R=100
beta<-c(1,array(seq(1,nreal),c(nreal,1)))
sx=seq(-1,1,by=0.01)

sX<-array(1,c(length(sx),1))
for (j in 1:p)
  sX<-cbind(sX,sx^j)
sd.w<-1
MISEmp<-NULL
MISE<-NULL
FPE<-NULL
for (r in 1:R){
  
  
  x<-sort(runif(N,min=-1,max=1))
  
  X<-array(1,c(N,1))
  for (j in 1:p)
    X<-cbind(X,x^j)
  
  xts=x #sort(runif(N,min=-1,max=1))
  Xts<-array(1,c(length(xts),1))
  for (j in 1:p.max)
    Xts<-cbind(Xts,xts^j)
  
  
  sf<-sX[,1:preal]%*%beta
  f<-X[,1:preal]%*%beta
  fts<-Xts[,1:preal]%*%beta
  Y<-f+rnorm(N,sd=sd.w)
  Yts<-fts+rnorm(N,sd=sd.w)
  
  
  XX<-X[,1:p]
  invX<-ginv(t(XX)%*%XX)
  beta.hat<-invX%*%t(XX)%*%Y
  Y.hat<-XX%*%beta.hat
  
  XXts<-Xts[,1:p]
  Y.hats<-XXts%*%beta.hat
 
  sXX<-sX[,1:p]
  sY.hat<-sXX%*%beta.hat
  
  var.hat=sum((Y-Y.hat)^2)/(N-p)
  
  mise<-NULL
  for (k in 1:length(xts))
    mise<-c(mise,(Yts[k]-Y.hats[k])^2)
  MISE<-c(MISE,mean(mise))
  
  
  emp<-NULL
  for (k in 1:length(x))
    emp<-c(emp,(Y[k]-Y.hat[k])^2)
  MISEmp<-c(MISEmp,mean(emp))
  
  FPE<-c(FPE,mean(emp)+2*(sum(emp)/(N-p))*p/N)
  
  plot(sx,sf,type="l",ylim=c(-4,8),lwd=2,
       main=paste("p=",p,"; MISE emp=",round(mean(MISEmp),2),
                  "; MISE=",round(mean(MISE),2),"; FPE=", round(mean(FPE),2)))
  points(x,Y,col="red",pch=1,lwd=3)
  lines(sx,sY.hat,col="red")
  points(xts,Yts,col="green",pch=2,lwd=3)
  legend("topright",c("training","test"),col=c("red","green"),
         pch=c(1,2),lwd=c(3,3))
  
  for (k in 1:length(xts)){
    segments(xts[k],Yts[k],xts[k],Y.hats[k],col="green")
    
  }
  
  for (k in 1:length(x)){
    segments(x[k],Y[k],x[k],Y.hat[k],col="red")
    
  }
  
}

