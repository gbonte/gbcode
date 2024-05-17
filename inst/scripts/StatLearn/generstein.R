## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

## Monte Carlo verification of the covariance penalty term.

rm(list=ls())
library(gbcode)
set.seed(0)

fun= function(X,sdw=1){
  N=NROW(X)
  n=NCOL(X)
  ## simple target function
  return(X[,1]*X[,2]^3+X[,n]+sdw*rnorm(N,0,1))
}
## Monte Carlo estimation of generalization error
N<-150 # number of examples
n<-3
order=10
sdw=0.5
Nstein=round(N/3)
S=1500 ## MC trials

R=NULL
Remp=NULL

Y=array(NA,c(N,S))
Yhat=array(NA,c(N,S))




deltaYhat=array(NA,c(Nstein,S))
Xtr<-array(runif(N*n,-1,1),c(N,n)) 
f=fun(Xtr,0)

for (s in 1:S){
  set.seed(s)
  Ytr=fun(Xtr,sdw)  
  
  Nts=5000
  Xts<-array(runif(Nts*n,-1,1),c(Nts,n))
  Yts=fun(Xts,sdw)
  
  ### parametric identification 
  ## complex polynomial model
  XXtr=NULL
  XXts<-NULL
  for (m in 1:order){
    XXtr<-cbind(XXtr,Xtr^m)
    XXts<-cbind(XXts,Xts^m)
  }
  RG=regrlin(XXtr,Ytr,
             XXts)
  
  Nsub<-sample(1:N,Nstein)
  for (jj in 1:Nstein){
    deltaY=numeric(N)
    dY=rnorm(1,sd=runif(1,0.01,0.1))
    deltaY[Nsub[jj]]=dY
    RGprime=regrlin(XXtr,Ytr+deltaY,
                    XXts)
    deltaYhat[jj,s]=c(((RGprime$Y.hat[Nsub[jj]]-RG$Y.hat[Nsub[jj]])/dY))
    ## derivative approximation
  }
  bestRemp=RG$MSE.emp
  
  Y[,s]=Ytr  #
  Yhat[,s]=c(RG$Y.hat)  #RG$Y.hat
  
  R=c(R,mean((Yts-RG$Y.hat.ts)^2))
  Remp=c(Remp,bestRemp)
  
  cat(".")
}
cat("\n")

## averaging of derivative
Der=NULL
for (i in 1:Nstein){
  Der=c(Der,mean(deltaYhat[i,]))
}


cat(paste("\n E[R(alpha_N)]=",mean(R),"\n"))
cat(paste("E[Remp(alpha_N)]=",mean(Remp),"\n"))
cat(paste("E[der]=",2*mean(Der)*(sdw^2),"\n"))

cat(paste("estimated=",mean(Remp)+2*mean(Der)*(sdw^2)))




