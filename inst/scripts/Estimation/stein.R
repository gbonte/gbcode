## Bias/variance tradeoff of a regularized estimator 
## muhatR= mu0+lambda*(mean(DN)-mu0)

rm(list=ls())
mu=2
sdz=2
mu0=0
N=50
muhat=NULL
muhatR=NULL
R=10000
lambda=0.9
for (r in 1:R){
  DN=rnorm(N,mu,sdz)
  muhat=c(muhat,mean(DN))
  muhatR=c(muhatR,mu0+lambda*(mean(DN)-mu0))
  
}

cat("B^2=",(mean(muhat)-mu)^2, "V=",var(muhat),"MSE=",mean((muhat-mu)^2), "\n")
cat("B^2=",(mean(muhatR)-mu)^2, "V=",var(muhatR),"MSE2=",mean((muhatR-mu)^2),"\n")



MSER=NULL
MSER2=NULL
LAM=seq (0.7,1,by=0.025)
for (lambda in LAM){
  muhatR=NULL
  muhatR2=NULL
  for (r in 1:R){
    DN=rnorm(N,mu,sdz)
    muhatR=c(muhatR,mu0+lambda*(mean(DN)-mu0))
    lambda2=(1-(N-3)/sum((DN-mean(DN)^2)))
    muhatR2=c(muhatR2,mu0+lambda2*(mean(DN)-mu0))
  }
  MSER=c(MSER,mean((muhatR-mu)^2))
  MSER2=c(MSER2,mean((muhatR2-mu)^2))
}

plot(LAM,MSER,xlab="lambda",ylab="MSE",main=paste("N=",N),type="l")
lines(LAM,MSER2,type="l",col="red")
abline(h=mean((muhat-mu)^2))