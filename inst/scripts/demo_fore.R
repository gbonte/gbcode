t=seq(0,4000,by=0.1)
N<-length(t)
H<-150 ## horizon prediction
TS<-sin(2*pi*t/20)+rnorm(N,sd=0.1)
TS.tr=TS[1:(N-H)]
N.tr<-length(TS.tr)
TS.ts<-TS[(N-H+1):N]
TS.tr=array(TS.tr,c(length(TS.tr),1))
Y.cont=multiplestepAhead(TS.tr,n=3, H=H,method="mimo")
Y.cont2=multiplestepAhead(TS.tr,n=3, H=H,method="stat_comb")

plot(t[(N-H+1):N],TS.ts)
lines(t[(N-H+1):N],Y.cont)
lines(t[(N-H+1):N],Y.cont2,col="red")
