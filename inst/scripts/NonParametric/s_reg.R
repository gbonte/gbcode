rm(list=ls())
theta=0
sdz=0.5
R=5000
MSE=NULL
MSE2=NULL
b=0.15
theta0=theta+b
N=15
lambdath=2*sdz^2/(N*b^2+sdz^2)

for (r in 1:R){
  lambda=lambdath/2 #0.8
  
  DN=rnorm(N,theta,sd=sdz)
  
  hatheta=mean(DN)
  hatheta2=lambda*theta0+(1-lambda)*hatheta
  MSE=c(MSE,(theta-hatheta)^2)
  MSE2=c(MSE2,(theta-hatheta2)^2)
}

cat("lambdath=",lambdath,
    "MSE=",mean(MSE),"MSE_reg=",mean(MSE2),"\n")