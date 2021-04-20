## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())

par(ask=TRUE)
X<-seq(-10,10,by=0.1)

beta0<--1
beta1<-1
N<-length(X)
R<-10000 ## number of MC trials
sd.w<-4
p<-2
beta.hat.1<-numeric(R)
beta.hat.0<-numeric(R)
var.hat.w<-numeric(R)
EE<-numeric(R)
MISE<-numeric(R)
Y.hat<-array(NA,c(R,N))
for (r in 1:R){
  Y<-beta0+beta1*X+rnorm(N,sd=sd.w)
  x.hat<-mean(X)
  y.hat<-mean(Y)
  S.xy<-sum((X-x.hat)*Y)
  S.xx<-sum((X-x.hat)^2)
  
  beta.hat.1[r]<-S.xy/S.xx
  beta.hat.0[r]<-y.hat-beta.hat.1[r]*x.hat
  
  Y.hat[r,]<-beta.hat.0[r]+beta.hat.1[r]*X
  
  EE[r]<-mean((Y-Y.hat[r,])^2) #empirical error
  var.hat.w[r]<-sum((Y-Y.hat[r,])^2)/(N-2)
  
  Yts<-beta0+beta1*X+rnorm(N,sd=sd.w) #test set
  MISE[r]<-mean((Yts-Y.hat[r,])^2)
}


MISEemp.th<-(1-p/N)*sd.w^2

print(paste("Expected empirical MISE: analytical=",MISEemp.th, "; simulated (MC) =",
            mean(EE) ))

MISE.th<-(1+p/N)*sd.w^2

print(paste("MISE: analytical =",MISE.th, "; simulated (MC) =",
            mean(MISE) ))

