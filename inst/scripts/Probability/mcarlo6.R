## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



## Monte Carlo estimation of P(x \in [a,b])

## 
rm(list=ls())
set.seed(0)
R=100000
# number of MC trials 
theta=NULL
X=NULL
Y=NULL
a=1
b=1.2
for ( r in 1:R){
  
  x=rnorm(1,0,2)
  if (x> a & x < b)
    y=1
  else
    y=0
  
  
  
  
  Y=c(Y,y)
  
}

cat("Est=",mean(Y),":",pnorm(b,0,2)-pnorm(a,0,2))
