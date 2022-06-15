## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


### script norm.R ###
set.seed(0)
N<-1000000
mu<-1
sigma<-2
DN<-rnorm(N,mu,sigma)
print(sum(DN<=(mu+sigma) & DN>=(mu-sigma))/N)
#[1] 0.683213
print(sum(DN<=(mu+1.645*sigma) & DN>=(mu-1.645*sigma))/N)
#[1] 0.900165
