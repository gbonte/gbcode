## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

# cumdis_1.R
# Script: visualizes the unbiasedness of the empirical distribution function 


par(ask=TRUE)
rm(list=ls())
N<-1000

I<-seq(-5,5,by=.1)
emp<-NULL
DN<-rnorm(3)
for (n in 4:N){
  
  DN<-c(DN,rnorm(10))
  F<-ecdf(DN)
  emp<-rbind(emp,F(I))
  m.emp<-apply(emp,2,mean) # average of the empirical function 
  plot(I,m.emp,main=paste(" Empirical distributions, N=", length(DN)))
  lines(I,pnorm(I),pch=15) #  distribution function
  legend(-4,1,legend=c("Empirical","Gaussian"),lty=c(3,1))


  
}

#m.emp<-apply(emp,2,mean) # average of the empirical function
#plot(I,m.emp)
#lines(I,pnorm(I),pch=15) #  distribution function
