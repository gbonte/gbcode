set.seed(7)

N <- 1000
beta_0 <- 1
beta_1 <- -1

w <- rnorm(n=N, mean=0, sd=sqrt(0.1))
x <- runif(N, min=-1, max=1)

g <- function(x){return(exp(x)/(1+exp(x)))}

y <- g(beta_0 + beta_1*x) + w

# maximum likelihood function
eml <- function(betas, x, y){
  N <- length(x)
  Lik <- 1
  
  for(i in 1:N){
    yh <- g(betas[1] + betas[2]*x[i]) ## error
    Lik <- Lik*dnorm(y[i], yh, sqrt(0.1))   
  }
  
  return(-log(Lik))
}

min<-optim(c(0,0),eml, x=x, y=y)
print(min$par)