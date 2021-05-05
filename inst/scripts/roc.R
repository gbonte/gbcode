## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


mu.p<-1
sd.p<-1

mu.n<--1
sd.n<-1


TT<-seq(-10,10,by=.05)
FPR<-numeric(length(TT))
SE<-numeric(length(TT))
PR<-numeric(length(TT))
N<-1000
DNp<-rnorm(N,mu.p,sd.p)
DNn<-rnorm(N,mu.n,sd.n)

for (tt in 1:length(TT)){
  thr<-TT[tt]
  
  FN<-length(which(DNp<thr))
  FP<-length(which(DNn>thr))
  TN<-length(which(DNn<thr))
  TP<-length(which(DNp>thr))
  FPR[tt]<-FP/(FP+TN)
  SE[tt]<-TP/(TP+FN)
  PR[tt]<-TP/(TP+FP)
}

par(mfrow=c(1,2))
plot(FPR,SE,type="l",col="red",main="ROC curve")
lines(FPR,FPR)
plot(SE,PR,type="l",col="red",main="PR curve")
