rm(list=ls())
library(gbcode)
options(warn = 3) 
t=seq(0,500,by=0.1)
N<-length(t)
H<-200 ## horizon prediction
TS<-sin(2*pi*t/20)+rnorm(N,sd=0.1)
TS.tr=TS[1:(N-H)]
N.tr<-length(TS.tr)
TS.ts<-TS[(N-H+1):N]
TS.tr=array(TS.tr,c(length(TS.tr),1))
Y.cont=multiplestepAhead(TS.tr,n=10, H=H,method="lazydirect",
                         detrend=0.5,XC=cbind(array(1:(N.tr+H),c(N.tr+H,1))))#,
                                                                        # (1:(N.tr+H))))
Y.cont2=multiplestepAhead(TS.tr,n=30, H=H,method="mimo")

plot(t[(N-H+1):N],TS.ts)
lines(t[(N-H+1):N],c(Y.cont),col="green")
lines(t[(N-H+1):N],c(Y.cont2),col="red")
