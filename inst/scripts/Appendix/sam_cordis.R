## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


## Samping distribution of sample correlation 

## 2 variables x and y are sampled where y=K*x
## The analytically derived correlation rho is compared to the sampling distribution of 
## the estimator rhohat of the correlation 
R=10000
N=50
varX=0.5
varW=1
K=1.33
rhohat=numeric(R)
for (r in 1:R){
  Dx=rnorm(N,sd=sqrt(varX))  ## Nor(0,1)
  Dy=K*Dx+rnorm(N,sd=sqrt(varW))  ## y=2x +w
  rhohat[r]=cor(Dx,Dy)
  
}

## E[xy]= E[Kx^2+w]=K*Var[x]
## Var[x]=VarX
## Var[y]=Var[2x+w]=K^2 Var[x]+var[w]

rho=K*varX/sqrt(varX*(K^2*varX+varW))

hist(rhohat,main=paste("Bias=",round(mean(rhohat)-rho,2)))
abline(v=rho,col="red")