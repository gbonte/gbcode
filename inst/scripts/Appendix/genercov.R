## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

## Monte Carlo verification of the covariance penalty term.

rm(list=ls())
library(gbcode)
set.seed(0)

fun= function(X,sdw=1){
  N=length(X)
  ## simple target function
  return(X+sdw*rnorm(N,0,1))
}
## Monte Carlo estimation of generalization error
N<-35 # number of examples
S=15000 ## MC trials
sdw=1
R=NULL
Remp=NULL

Y=array(NA,c(N,S))
Yhat=array(NA,c(N,S))
Xtr<-runif(N,-1,1) 
f=fun(Xtr,0)

for (s in 1:S){
  Ytr=fun(Xtr,sdw)  
    
  Nts=50000
  Xts<-runif(Nts,-1,1) 
  Yts=fun(Xts,sdw)
  
  ### parametric identification 
  ## complex polynomial model
  RG=regrlin(cbind(Xtr,Xtr^2,Xtr^3,Xtr^4,Xtr^5,Xtr^6,Xtr^7,Xtr^8),Ytr,
             cbind(Xts,Xts^2,Xts^3,Xts^4,Xts^5,Xts^6,Xts^7,Xts^8))
  
 # RG=regrlin(cbind(Xtr,Xtr^2,Xtr^3),Ytr,
  #                      cbind(Xts,Xts^2,Xts^3))
  bestRemp=RG$MSE.emp
  
 
  Y[,s]=Ytr  #
  Yhat[,s]=c(RG$Y.hat)  #RG$Y.hat
  
   
  R=c(R,mean((Yts-RG$Y.hat.ts)^2))
  Remp=c(Remp,bestRemp)
  
  cat(".")
}

## covariance computation
Cov=NULL
for (i in 1:N)
  Cov=c(Cov,mean((Y[i,]-f[i])*(Yhat[i,]-f[i])))


cat(paste("\n E[R(alpha_N)]=",mean(R),"\n"))
cat(paste("E[Remp(alpha_N)]=",mean(Remp),"\n"))
cat(paste("E[Cov]=",mean(Cov),"\n"))

cat(paste("estimated=",mean(Remp)+2*mean(Cov)))




