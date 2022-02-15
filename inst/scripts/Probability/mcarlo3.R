## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



## Monte Carlo validation of Jensen's inequality f(E[x))<= E[f(x)]
## with f=x^2-x
## Let X~Unif(-2,5) -> E[X]=(b+a)/2 , Var(X)=1/12*(b-a)^2 =1/3=E[X^2]-E[X]^2
## 

set.seed(0)
R=10000
# number of MC trials 

f<-function(x){
  -x^2
}

FX=NULL
X=NULL

a=1
b=1

for ( r in 1:R){
  
  x=rnorm(1,a,b)
  fx=f(x)
  
  FX=c(FX,fx)
  X=c(X,x)  
  
}
muX=mean(X)
cat("E[f(x)]=",mean(FX))
cat("\n f(E[x])=",f(muX))