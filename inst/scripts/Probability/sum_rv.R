## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

# sum_rv.R
# Script: shows the relation between the variance
# of z and the variance of z.bar=(sum of N i.i.d. random variables)


rm(list=ls())
graphics.off()
R<-10000 #number of realizations of each variable
N<-100 # number odf summed variables
par(mfrow=c(1,1))
par(ask=TRUE)
sdev<-1
mu<-1
z<-rnorm(R,mean=mu,sd=sdev) # D is uniformly distributed
cat("Var[z]=",var(z),"\n")
hist(z,main=paste("single r.v.: mean=",
                  mu," variance=",sdev^2)) # see the shape of z


SN<-rep(0,R)
for (n in 1:N){
  SN<-SN+rnorm(R,mean=mu,sd=sdev)
  
}

cat("Var[Sum(z)]/Var[z]=",(var(SN)/var(z)),"\n")
hist(SN,main=paste("Sum of ",N, " r.v.s (mu=",mu,",var=",sdev^2 ,"): variance ~", 
                      round(var(SN)))) # see the shape of SN

zbar<-SN/N
hist(zbar,main=paste("Average of ",N, " r.v.s (mu=",mu,",var=",sdev^2 ,"): variance ~", 
                     round(var(zbar),digits=4))) # see the shape of z.bar

