## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


### script norm.R ###
set.seed(0)
N<-1000000
mu<-1
sigma<-2

DN<-rnorm(N,mu,sigma)

for (size in c(1,1.282,1.645,1.96,2,2.57,3))
cat("P[mu-",size,"sigma <= z <=  mu+", size, "sigma]",  " Monte Carlo: ", sum(DN<=(mu+size*sigma) & DN>=(mu-size*sigma))/N, 
    "analytical:", pnorm(mu+size*sigma,mu,sigma)-pnorm(mu-size*sigma,mu,sigma),"\n")
