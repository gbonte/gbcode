## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

# chisq.R
# Script: plot the chi-squared density and cumulative distribution


N<-10
x<-seq(0,50,by=.1)
plot(x,dchisq(x,N),main=paste("chi-squared (N=" ,N,") density"))


plot(x,pchisq(x,N),main=paste("chi-squared (N=" ,N,") cumulative distribution"))



