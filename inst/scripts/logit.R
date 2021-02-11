logit<-function(x){
  return(exp(x)/(1+exp(x)))
}

N=100

beta0=runif(1)
beta1=-runif(1)

X=rnorm(N)
Y=numeric(N)
for (i in 1:N){
  pi=logit(beta0+beta1*X[i])
  Y[i]=sample(c(0,1),1,prob=c(1-pi,pi))
  
}


B=seq(-2,2,by=0.05)
maxLik=-Inf
for (betahat0 in B){
  for (betahat1 in B){
    Lik=1
    for (i in 1:N){
      p1=logit(betahat0+betahat1*X[i])
      if (Y[i]==1)
        Lik=Lik+log(p1)
      else
        Lik=Lik+log((1-p1))
    }
    if (Lik > maxLik){
      maxLik=Lik
      #print(Lik)
      bestbetahat0=betahat0
      bestbetahat1=betahat1
      
    }
    
    
  }
}

cat("beta0=",beta0, bestbetahat0,"\n")
cat("beta1=",beta1, bestbetahat1,"\n")