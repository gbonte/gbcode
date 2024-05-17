## Monta Carlo comparison of a linear combination of Gaussians and 
## a mixture of Gaussians

rm(list=ls())

mu1=runif(1,-2,-1)
std1=runif(1,0.2,0.5)

w1=0.4

mu2=2
std2=0.3
w2=1-w1

R=50000
Z1=NULL ## linear combination of Gaussians
Z2=NULL ## mixture of Gaussians

cat("\n Progressing with MC simulation \n")
pb <- txtProgressBar(0, R, style = 3)

for (r in 1:R){
  Z1=c(Z1,w1*rnorm(1,mu1,std1)+w2*rnorm(1,mu2,std2))
  if (runif(1)<w1)
    Z2=c(Z2,rnorm(1,mu1,std1))
  else
    Z2=c(Z2,rnorm(1,mu2,std2))
  setTxtProgressBar(pb, r)
  
}

cat("\n Mean comb=",mean(Z1), "; Mean mixture=", mean(Z2),"\n")

par(mfrow=c(1,2))
hist(Z1,breaks=50,main="linear combination of 2 gaussians")
hist(Z2,breaks=50,main="mixture of 2 gaussians")