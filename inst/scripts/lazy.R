
rm(list=ls())
source("dopler.R")
library(lazy)

N<-2000
D<-dataset.dopler(2000)
plot(D$x,D$y,type="l")

d<-data.frame(D$y,D$x)
names(d)<-c("Y","X")

MSE=NULL

Nn=seq(3,50,by=1)
for (number.neighbors in Nn){
  
  mod.lazy<-lazy(Y~.,d,
                 control=lazy.control(conIdPar=NULL,
                                      linIdPar=c(number.neighbors,number.neighbors), quaIdPar=NULL))
  
  d.ts<-data.frame(D$y.ts,D$x.ts)
  names(d.ts)<-c("Y","X")
  p<-predict(mod.lazy,d.ts)
  
  MSE=c(MSE,mean((p$h-D$y.ts)^2))
  
  plot(D$x.ts,D$y.ts,type="l", main=paste("Number neighbors=",number.neighbors))
  lines(D$x.ts,c(p$h),col="red")
  Sys.sleep(.1)
}
plot(Nn,MSE,type="l",ylab="MISE",xlab="Number of neighbors")
