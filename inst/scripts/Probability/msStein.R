rm(list=ls())
## Monte Carlo validation of 
n=13
g<-function(z){
  list(g=3*z^3,dg=9*z^2)
}

R=5000
sdw=0.3
mu=1
P<-NULL
D<-NULL
for (r in 1:R){
  z<-rnorm(n,mean=mu,sd=sdw)
  G<-g(z)
  P<-c(P,(z-mu)%*%G$g)
  D<-c(D,sdw^2*sum(G$dg))
}

print(mean(P))
print(mean(D))