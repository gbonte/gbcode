rm(list=ls())
library(gbcode)


Ntr=50
H=10
TS=sin(seq(1,Ntr+H))

Its=seq(Ntr+1,Ntr+H)
TS.tr=TS[1:Ntr]
TS.ts=TS[(Ntr+1):(Ntr+H)]
maxn=4
maxC=10
NMSE=array(Inf,c(10,maxC))


for (C in seq (3,maxC)){
  for (n in seq (1,maxn)){
    TS.hat=multiplestepAhead(TS.tr,n=n,C=C,H=H,method="timefit")
    if (length(TS.hat)!=length(TS.ts))
      browser()
    e=c(TS.hat)-c(TS.ts)
    
    NMSE[n,C]=mean(e^2)/var(TS.tr)
    
  }
}
inds = which(NMSE == min(NMSE), arr.ind=TRUE)[1,]  
print(inds)
print(min(NMSE))
#plot(TS.tr,type="l",ylim=c(-10,10),xlim=c(1,Ntr+H),main=paste("NMSE=",NMSE))


#lines(Its,TS.hat,col='red',lwd=4)