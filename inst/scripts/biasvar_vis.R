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

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - lm.influence(linear.model)$hat)
  mean(pr^2)
}
set.seed(1)

n<-1
N<-25

x<-seq(-2,2,length.out=N)
N<-length(x)
sd.w<-0.75
O<-3
Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
data.tr<-cbind(Y,x)


R<-20

Remp<-numeric(R)
MSE.loo<-numeric(R)
x.ts<-seq(-2,2,length.out=200)

B2<-numeric(R)
V<-numeric(R)
PR<-numeric(R)
FPE<-numeric(R)
for (r in 1:R){
  X.ts<-NULL
  X<-NULL
  for (ord in 1:r){
    X<-cbind(X,x^ord)
    X.ts<-cbind(X.ts,x.ts^ord)
  }
  p<-r+1
  Pr<-NULL
  for (rr in 1:500){
    set.seed(rr)
    Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
    DN<-data.frame(cbind(Y,X))
    
    mod<-lm(Y~.,DN)
    sd.w.hat<-sqrt(sum(mod$residuals^2)/(N-p))
    if (rr==1){
      Remp[r]<-(mean(mod$residuals^2))
      PR[r]<-PRESS(mod)
      FPE[r]<-Remp[r]+2*sd.w.hat*p/N
    }
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
  title(paste("N=", N, "; degree=",r, "\n Bias=",round(mean(abs(Y.ts-apply(Pr,1,mean))),2),
              "; Var=",round(mean(apply(Pr,1,var)),2), 
              "; Emp risk=",round(Remp[r],2)))
  B2[r]=mean((Y.ts-apply(Pr,1,mean))^2)
  V[r]=mean(apply(Pr,1,var))
  par(ask=TRUE)
}
par(ask=FALSE)
mR=13
plot(1:mR,Remp[1:mR],type="l",main="Bias-variance tradeoff",
     lwd=1, xlab="degree",ylab="",col="yellow",ylim=c(0,4))
lines(1:mR,B2[1:mR]+V[1:mR],col="black",lwd=3)
lines(1:mR,B2[1:mR],col="green")
lines(1:mR,V[1:mR],col="red")
lines(1:mR,PR[1:mR],col="orange")
lines(1:mR,FPE[1:mR],col="cyan")
legend("topright", legend=c("Remp","MSE","Bias","Variance","LOO","FPE"),
col = c("yellow","black","green","red","orange","cyan"),
       lty = c(1,1,1,1,1))
