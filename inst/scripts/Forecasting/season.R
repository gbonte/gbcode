## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
library(Ecdat)
data(AirPassengers)
add=TRUE
geom=FALSE
library(fpp)
data(ausbeer)
S=AirPassengers
S=ausbeer
S=sin(0.5*seq(1,500,by=1))
plot(S,type="l")
N=length(S)
trnd=lm(S ~ seq(S))$fit

if (add)
  TS<-S-trnd
if (geom)
  TS<-S/trnd
PVS=numeric(100)+Inf
for (s in 2:100){
  PV=NULL
  
  m_S = t(matrix(data = TS, nrow = s))
  for (i in 1:s){
    xs=m_S[,s]
    ys=unlist(m_S[,-s])
    PV=c(PV,t.test(xs,ys)$p.value)
    
  }
  PVS[s]=median(PV)
  
}
if (min(PVS)<0.05){
  s=which.min(PVS)
  cat("Seasonality at:",s,"\n")
  m_S = t(matrix(data = TS, nrow = s))
  TSeas=rep(apply(m_S,2,mean),length.out=N)
  lines(TSeas,col="red")
  TSdeseas=TS-TSeas
  lines(TSdeseas,col="green",lwd=3)
}