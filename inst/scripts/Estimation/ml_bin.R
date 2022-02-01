## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



rm(list=ls())
N<-11
z<-7
p<-seq(.01,1-.01,by=.01)

par(ask=TRUE)
L<-lapply(p,dbinom,size=N,x=z)
plot(p,as.numeric(L),main="Likelihood",type="l",xlab="Probability p",ylab="L(p)")

abline(v=z/N,lty=2)

plot(p,log(as.numeric(L)),main="Log-Likelihood",type="l",xlab="Probability p",ylab="l(p)")
abline(v=z/N,lty=2)

