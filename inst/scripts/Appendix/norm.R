## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

### script norm.R ###

set.seed(0)
N<-1000000
mu<-1
sigma<-2
DN<-rnorm(N,mu,sigma)

print("Enter size of interval as a constant times variance ")
while (TRUE){
  n <- as.numeric(readline(prompt="Enter constant : "))
  cat("probability=",sum(DN<=(mu+n*sigma) & DN>=(mu-n*sigma))/N,"\n")
}


