## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

# NonLinear model (Neural network)
# Nonlinear function




library(lazy)
par(ask=TRUE)
f<-function(x,sd=0){
  # y<-3*((1-x)^2)*exp(-(x^2))- 10*(x/5 - x^3 )*exp(-x^2) - 1/3*exp(-(x+1)^2 )+sd*rnorm(length(x))
  y<-x^2+sd*rnorm(length(x))
  y
}

set.seed(0)
X<-seq(-3,3,by=.3)
N<-length(X)
n<-1
R<-100
Y.hat<-array(NA,c(R,N))

for (n.par in 3:100){
  for (r in 1:R){
    Y<-f(X,1)
    data.tr<-data.frame(cbind(Y,X))
    names(data.tr)[1]<-'Y'
    names(data.tr)[2:(n+1)]<-letters[1:n]
    mod.lazy<-lazy(Y~.,data=data.tr,
                   control=lazy.control(linIdPar=c(n.par,n.par)))
    q<-data.frame(X)
    names(q)<-letters[1:n]
    ll<- predict(mod.lazy,q)
    Y.hat[r,]<- ll$h
    
  }
  
  avg.var<-round(mean(apply(Y.hat,2,"var")),2)
  regr<-apply(Y.hat,2,"mean")
  BIAS<-round(sum((f(X)-regr)^2),2)
  plot(X,Y.hat[1,],
       type="l",
       main=paste("Var:",avg.var ,"Bias:",BIAS, " n. par=",n.par),
       ylim=c(-3,6))
  for (r in 2:R){
    lines(X,Y.hat[r,])
  }
  lines(X,f(X),col="red")
  lines(X,regr,col="green")
  
  
}



