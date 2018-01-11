library(gbcode)
t=seq(0,20,by=1)
N<-length(t)
H<-5 ## horizon prediction
TS<-sin(t)+rnorm(N,sd=0.1)
TS.tr=TS[1:(N-H)]
N.tr<-length(TS.tr)
TS.ts<-TS[(N-H+1):N]
TS.tr=array(TS.tr,c(length(TS.tr),1))
Y.cont=multiplestepAhead(TS.tr,n=3, H=H,method="mimo",C=5)
