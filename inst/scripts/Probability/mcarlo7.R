## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



## Monte Carlo estimation of parameters

R=50000
# number of MC trials 

Z2=NULL
a=-1
b=1
muz=(b+a)/2
for ( r in 1:R){
  z=runif(1,a,b)
  Z2=c(Z2,(z-muz)^2)
  
}

cat("Var. th=", (b-a)^2/12, "MC approximation= ",mean(Z2))


