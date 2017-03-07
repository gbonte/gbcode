## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
x<-seq(-5,5,by=0.05)
plot(dnorm(x),type="l")
lines(dt(x,df=10),type="l",col="red")
lines(dt(x,df=3),type="l",col="green")
qnorm(0.05,lower.tail=F)
qt(0.05,lower.tail=F,df=10)


plot(x,dnorm(x),type="l")

abline(v=qnorm(0.025,lower.tail=FALSE),lty=2)
abline(v=qnorm(0.025,lower.tail=TRUE),lty=2)