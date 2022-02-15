## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



## Monte Carlo estimation of correlation

## 
rm(list=ls())
set.seed(0)
R=10000
# number of MC trials 
XY=NULL
X=NULL
Y=NULL
for ( r in 1:R){
  
  x=rnorm(1,0,1)
  y=2*x^2
  
  
  XY=c(XY,x*y)
  X=c(X,x)  
  Y=c(Y,y)
  
}
muX=mean(X)
sdX=sd(X)
sdY=sd(Y)
cat("rho[xy]=",mean(XY)/(sdX*sdY))