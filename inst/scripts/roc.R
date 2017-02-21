## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


mu.p<-2
sd.p<-1

mu.n<--1
sd.n<-1


TT<-seq(-10,10,by=.05)
FPR<-numeric(length(TT))
SE<-numeric(length(TT))
R<-100000
for (tt in 1:length(TT)){
  thr<-TT[tt]
  DNp<-rnorm(R,mu.p,sd.p)
  DNn<-rnorm(R,mu.n,sd.n)
  FN<-length(which(DNp<thr))
  FP<-length(which(DNn>thr))
  TN<-length(which(DNn<thr))
  TP<-length(which(DNp>thr))
  FPR[tt]<-FP/(FP+TN)
  SE[tt]<-TP/(TP+FN)
}


plot(FPR,SE)

