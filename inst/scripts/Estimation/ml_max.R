## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



rm(list=ls())

n=1
mx=1

th=seq(-2000,0,by=0.1)
sdw=1
thhat=NULL

for (i in 1:100){
  n=n+1
  L=n*(pnorm(mx-th,sd=sdw))^(n-1)*dnorm(mx-th,sd=sdw)
  # from Y. Pawitan book page 26
  thhat=c(thhat,th[which.max(L)])
  
}


plot(thhat,xlab="# samples",ylab="ml estimator of mean given the max")