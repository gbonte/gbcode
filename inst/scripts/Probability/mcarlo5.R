## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



## Monte Carlo estimation of E[max(X,Y)]

## 
rm(list=ls())
set.seed(0)
R=10000
# number of MC trials 
theta=NULL
X=NULL
Y=NULL
for ( r in 1:R){
  
  x=rnorm(1,0,2)
  y=runif(1,-1,1)
  
  
  
  theta=c(theta,max(c(x,y)))
  X=c(X,x)  
  Y=c(Y,y)
  
}
muX=mean(X)
sdX=sd(X)
sdY=sd(Y)
cat("Est=",mean(theta))
hist(theta)