## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

# sum_rv.R
# Script: shows the relation between the variance
# of z and the variance of z.bar=(sum of N i.i.d. random variables)

sum_rv<-function(){
  rm(list=ls())
  R<-10000 #number of realizations of each variable
  N<-1000 # number odf summed variables
  par(ask=TRUE)
  sdev<-10
  mu<-1
  z<-rnorm(R,mean=mu,sd=sdev) # D is uniformly distributed
  print(var(z))
  hist(z,main=paste("single r.v.: mean=",mu," variance=",sdev^2)) # see the shape of z


  z.bar<-rep(0,R)
  for (n in 1:N){
    z.bar<-z.bar+rnorm(R,mean=mu,sd=sdev)

  }

  print(var(z.bar)/var(z))
  hist(z.bar,main=paste("Sum of ",N, " r.v.s (mu=",mu,",var=",sdev^2 ,"): variance ~", round(var(z.bar)))) # see the shape of z.bar

  aver<-z.bar/N
  hist(aver,main=paste("Average of ",N, " r.v.s (mu=",mu,",var=",sdev^2 ,"): variance ~", round(var(aver),digits=1))) # see the shape of z.bar
}
