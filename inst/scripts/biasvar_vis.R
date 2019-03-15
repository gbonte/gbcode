## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

## Visualization of bias variance tradeoff for a polynomial fitting
rm(list=ls())


f<-function(x,ord){
  f<-1
  for (i in 1:ord)
    f<-f+(x^i)
  
  f
}


set.seed(1)

n<-1
N<-25

x<-seq(-2,2,length.out=N)
N<-length(x)
sd.w<-0.5
O<-3
Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
data.tr<-cbind(Y,x)


R<-20

Remp<-numeric(R)
FPE<-numeric(R)
PSE<-numeric(R)
MSE.loo<-numeric(R)
x.ts<-seq(-2,2,length.out=200)





for (r in 1:R){
  X.ts<-NULL
  X<-NULL
  for (ord in 1:r){
    X<-cbind(X,x^ord)
    X.ts<-cbind(X.ts,x.ts^ord)
  }
  p<-r+1
  Pr<-NULL
  for (rr in 1:200){
    Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
    DN<-data.frame(cbind(Y,X))
    
    mod<-lm(Y~.,DN)
    sd.w.hat<-sqrt(sum(mod$residuals^2)/(N-p))
    Remp[r]<-sqrt(mean(mod$residuals^2))
    
    
    e.hat.i<-numeric(N)
    
    
    
    Y.ts<-f(x.ts,ord=O)
    data.ts<-data.frame(cbind(Y.ts,X.ts))
    names(data.ts)<-names(DN)
    if (rr==1){
      plot(x.ts,Y.ts,type="l",ylim=c(min(Y),max(Y)))
      points(x,Y)
    }
    pr<-predict(mod,data.ts)
    Pr<-cbind(Pr,pr)
    lines(x.ts,pr,col="red")
    
    
  }
  lines(x.ts,Y.ts,type="l",ylim=c(min(Y),max(Y)),lwd=5)
  lines(x.ts,apply(Pr,1,mean),type="l",ylim=c(min(Y),max(Y)),lwd=5,col="green")
  title(paste("Bias",round(mean(abs(Y.ts-apply(Pr,1,mean))),2),
              "Var",round(mean(apply(Pr,1,var)),2), "degree=",r))
  cat(r,"\n")
  par(ask=TRUE)
}

