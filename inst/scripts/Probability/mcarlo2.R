## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



## Monte Carlo approximation of covariance
## COV(X,Y)=E[XY]-E[X]E[Y]

## Let X~Unif(a,b) -> E[X]=(b+a)/2 , Var(X)=1/12*(b-a)^2 =1/3=E[X^2]-E[X]^2
## Let Y=K*X
## COV(X,Y)=E[XY]-K*E[X]E[X]=E[K*X^2]-K(E[X]^2)=K(Var(X)+E[X]^2)-K (E[X])^2= K*Var(X)
R=50000
# number of MC trials 

distr="uniform"

XY=NULL
X=NULL

K=2
if (distr=="uniform"){
  a=1
  b=20
  VX=1/12*(b-a)^2
} else{
  mu=1
  sigma=1
  VX=sigma^2
  
}
for ( r in 1:R){
  if (distr=="uniform")
    x=runif(1,a,b)
  else
    x=rnorm(1,mu,VX)
  y=K*x
  XY=c(XY,x*y)
  X=c(X,x)  
  
}
cat("Analytical covariance=",K*VX)
cat("\n MonteCarlo covariance=",(mean(XY)-K*mean(X)^2))