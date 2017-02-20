## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi



# cumdis_2.R
# Script: visualizes the unbiasedness of the empirical distribution function

library(stepfun)
par(ask=TRUE)
rm(list=ls())
N<-100
R<-100
I<-seq(-5,5,by=.1)
emp<-NULL
for (i in 1:R){
  
  DN<-rnorm(N)
  F<-ecdf(DN)
  emp<-rbind(emp,F(I))
  m.emp<-apply(emp,2,mean) # average of the empirical function
  plot(I,m.emp,main=paste("Average of ",i," empirical distributions"))
  lines(I,pnorm(I),pch=15) #  distribution function
  legend(-4,1,legend=c("Empirical","Gaussian"),lty=c(3,1))
  
  
  
}

#m.emp<-apply(emp,2,mean) # average of the empirical function
#plot(I,m.emp)
#lines(I,pnorm(I),pch=15) #  distribution function

