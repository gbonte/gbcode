## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi
rm(list=ls())
source("inst/scripts/dopler.R")
N=2000
library(nnet)
D<-dataset.dopler(N)

D$y=D$x^3
plot(D$x,D$y,type="l")

d<-data.frame(D$y,D$x)
names(d)<-c("Y","X")

for (number.nodes in 1:30){
  
  mod.nn<-nnet(Y~.,data=d,size=number.nodes,skip=FALSE,
               trace=T, maxit=3000,linout=TRUE,rang=0.2)
  d.ts<-data.frame(D$y.ts,D$x.ts)
  names(d.ts)<-c("Y","X")
  p<-predict(mod.nn,d.ts)
  
  
  
  plot(D$x.ts,D$y.ts,type="l", main=paste("Number hidden nodes=",number.nodes))
  lines(D$x.ts,p,col="red")
  browser()
}


