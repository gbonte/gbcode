## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi
rm(list=ls())

mu.p<-1
sd.p<-1

mu.n<--mu.p
sd.n<-1


TT<-seq(1,0,by=-.001)
## threshold
FPR<-numeric(length(TT))
SE<-numeric(length(TT))
FPR2<-numeric(length(TT))
SE2<-numeric(length(TT))
PR<-numeric(length(TT))
AL<-numeric(length(TT))
N<-5000

DNp<-rnorm(N/2,mu.p,sd.p)
DNn<-rnorm(N/2,mu.n,sd.n)
X<-c(DNn,DNp)
Y<-numeric(N)+1
Y[1:(N/2)]=-1

pb<-FALSE
for (r in seq(1,20,by=5)){
  
  Phat<-numeric(N)
  rr=max(1,r-1)
  for (i in 1:N){
    Ph<-(dnorm(X[i],mu.p,sd.p)/(dnorm(X[i],mu.p,sd.p)+dnorm(X[i],mu.n,sd.n)))
    if(!pb){
      Phat[i]=1-pbinom(rr-1,r,Ph) ## probability that on r repetitions, more than rr are positive
      ## + if #+> rr, -  else
    } else {
      Phat[i]=Ph
    }
  }
  AUROC=0
  for (tt in 1:length(TT)){
    thr<-TT[tt]
    
    FN<-length(which(Phat<thr & Y>0))
    FP<-length(which(Phat>thr& Y<0))
    TN<-length(which(Phat<thr& Y<0))
    TP<-length(which(Phat>thr& Y>0))
    FPR[tt]<-FP/(FP+TN)
    SE[tt]<-TP/(TP+FN)
    PR[tt]<-TP/(TP+FP)
    AL[tt]<-(TP+FP)/(N)
    
    p1=FPR[tt] # fpr P(+ | -)=FPR
    p2=1-SE[tt] #fnr P(- | +)=FNR=1-TPR
    
    FPR2=FPR
    SE2=SE
    if (pb){
      FPR2[tt]=1-pbinom(rr-1,r,p1)  
      ## FPR2= 1-pbinom(rr-1,r,p1) = 1-  Pbinom(#+ <= rr | -) = Pbinom(#+ > rr | -)
      
      SE2[tt]=1-pbinom(rr-1,r,1-p2)
      ## FNR2= pbinom(rr-1,r,1-p2) = Pbinom(#+ <= rr-1 | +) = Pbinom(#+ < rr | +)
    }
  }
  
  par(mfrow=c(1,1))
  plot(FPR,SE,type="l",col="red",main="ROC curve",ylab="SE (TPR)")
  lines(FPR,FPR)
  lines(FPR2,SE2,col="green")
  
  print(integrate(approxfun(FPR2,SE2,method="constant"),0,1,subdivisions = 1000))
}