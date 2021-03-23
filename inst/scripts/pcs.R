rm(list=ls())
N=50

aMSE1=NULL
aMSE2=NULL
aMSE3=NULL
aV1=NULL
aV2=NULL
aV3=NULL

aB1=NULL
aB2=NULL
aB3=NULL

for (i in 1:100){
  sdw=runif(1,0.1,2)
  sigma1=runif(1,sdw/2,sdw)
  sigma2=runif(1,sdw/2,sdw)
  theta=runif(1,-2,2)
  B1=runif(1,-sdw,sdw)
  B2=runif(1,-sdw,sdw)
  MSE1=B1^2+sigma1^2
  MSE2=B2^2+sigma2^2
  thetahat3=NULL
  thetahat4=NULL
  
  for (r in 1:10000){
    y=theta+rnorm(1,sd=sdw)
    thetahat1=rnorm(N,theta+B1,sigma1)
    thetahat2=rnorm(N,theta+B2,sigma2)
    
    Ve=var(thetahat1-y)
    Ee2=mean((thetahat1-y)^2)
    Ve2=var((thetahat1-y)^2)
    
    eB1=Ee2-Ve
    eB2=0.5*(Ve2/(2*Ve)-2*Ve)
    
    browser()
    
    
    if (mean((thetahat2-y)^2)<mean((thetahat1-y)^2)) 
      thetahat3=c(thetahat3,rnorm(1,theta+B2,sigma2))
    else
      thetahat3=c(thetahat3,rnorm(1,theta+B1,sigma1))
    thetahat4=c(thetahat4,mean(rnorm(1,theta+B1,sigma1),rnorm(1,theta+B2,sigma2)))
    
  }
  aV1=c(aV1,min(sigma1^2,sigma2^2))
  aV2=c(aV2,max(sigma1^2,sigma2^2))
  aV3=c(aV3,var(thetahat3))
  aV4=c(aV4,var(thetahat4))
  
  aB1=c(aB1,min(B1^2,B2^2))
  aB2=c(aB2,max(B1^2,B2^2))
  aB3=c(aB3,(mean(thetahat3)-theta)^2)
  
  aMSE1=c(aMSE1,min(MSE1,MSE2))
  aMSE2=c(aMSE2,max(MSE1,MSE2))
  aMSE3=c(aMSE3,mean((thetahat3-theta)^2))
}

boxplot(cbind(aB1,aB2,aB3))
