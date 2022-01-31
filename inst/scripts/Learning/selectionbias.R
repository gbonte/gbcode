## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
library(gbcode)
f<- function(x){
  return(0.15*x)
} 


N=15
Nts=10000
S=1000
sdw=0.5


Ets1=NULL  ## generalization error of h1
Ets2=NULL  ## generalization error of h2
Ets=NULL   ## generalization error of selected model

for (s in 1:S){
  
  X=rnorm(N,sd=0.5)
  Y=f(X)+rnorm(N,sd=sdw)
  Xts=rnorm(Nts)
  Yts=f(Xts)+rnorm(Nts,sd=sdw)
  
  Eloo1=NULL
  Eloo2=NULL
  for (i in 1:N){
    I=setdiff(1:N,i)
    hi1=mean(Y[I])
    ei1=Y[i]-hi1
    Eloo1=c(Eloo1,ei1^2)
    
    hi2=regrlin(X[I],Y[I],X[i])$Y.hat.ts
    ei2=Y[i]-hi2
    Eloo2=c(Eloo2,ei2^2)
    
  }
  MSEloo1=mean(Eloo1)
  MSEloo2=mean(Eloo2)
  
  h1=mean(Y)
  Ets1=c(Ets1,mean((Yts-h1)^2))
  
  h2=regrlin(X,Y,Xts)$Y.hat.ts
  Ets2=c(Ets2,mean((Yts-h2)^2))
  
  if (MSEloo1<MSEloo2)
    Ets<-c(Ets,mean((Yts-h1)^2)) ## h1 is the selected model
  else
    Ets<-c(Ets,mean((Yts-h2)^2)) ## h2 is the selected model
  
  
}

cat("G1=",mean(Ets1), "G2=",mean(Ets2), "G selected=",mean(Ets))