## Monte Carlo estimation of functional risk 
rm(list=ls())

sdw=1

Nts=1000000
Xts<-runif(Nts,-2,2) 
Yts=Xts^3+rnorm(Nts,0,sdw) 

R=NULL
bestR=Inf
A=seq(1,3.5,by=0.01)
for (alpha in A){
  Ra=mean((Yts-alpha*Xts)^2)
  R=c(R,Ra)
  if (Ra<bestR){
    bestR=Ra
    bestalpha=alpha
  }
  cat(".")
}


plot(A,R)
lines(A,1/4*(16*A^2/3-128*A/5+256/7)+1,col="red")

cat(paste("R(alpha)=",bestR, "best alpha=",bestalpha ))