library(gbcode)
t=seq(0,400,by=0.1)
N<-length(t)
H<-50 ## horizon prediction
TS<-sin(t)+rnorm(N,sd=0.1)
TS.tr=TS[1:(N-H)]
N.tr<-length(TS.tr)
TS.ts<-TS[(N-H+1):N]
TS.tr=array(TS.tr,c(length(TS.tr),1))
Y.cont=multiplestepAhead(TS.tr,n=20, dummy=10,H=H,method="mimo.comb")

plot(t[(N-H+1):N],TS.ts)
lines(t[(N-H+1):N],Y.cont,col="red")