## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi
rm(list=ls())
set.seed(0)
N=10
DN=rnorm(N,0,1)


Theta=seq(-2,2,by=0.001)
L=numeric(length(Theta))+1
for (ith in 1:length(Theta)){
  for (i in 1:N)
    L[ith]=L[ith]*dnorm(DN[i],Theta[ith],1)
  
}
plot(Theta,L)

cat("Sample average=",mean(DN), 
    "arg max L=", Theta[which.max(L)],"\n")
