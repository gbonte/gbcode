## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


mu.p<-1
sd.p<-1

mu.n<--1
sd.n<-1


TT<-seq(-10,10,by=.01)
FPR<-numeric(length(TT))
SE<-numeric(length(TT))
PR<-numeric(length(TT))
AL<-numeric(length(TT))
N<-2000
DNp<-rnorm(N/2,mu.p,sd.p)
DNn<-rnorm(N/2,mu.n,sd.n)

for (tt in 1:length(TT)){
  thr<-TT[tt]
  
  FN<-length(which(DNp<thr))
  FP<-length(which(DNn>thr))
  TN<-length(which(DNn<thr))
  TP<-length(which(DNp>thr))
  FPR[tt]<-FP/(FP+TN)
  SE[tt]<-TP/(TP+FN)
  PR[tt]<-TP/(TP+FP)
  AL[tt]<-(TP+FP)/(N)
}

par(mfrow=c(1,3))
plot(FPR,SE,type="l",col="red",main="ROC curve",ylab="SE (TPR)")
lines(FPR,FPR)
plot(SE,PR,type="l",col="red",main="PR curve",xlab="SE (TPR)")

plot(AL,SE,type="l",col="red",main="Lift curve",ylab="SE (TPR)",
     xlab="% alerts")
lines(AL,AL)
