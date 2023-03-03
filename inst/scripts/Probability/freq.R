## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

##  Fair coin tossing random experiment
## Evolution of the relative frequency (left) 
## and of the absolute difference between the number of heads and tails 

set.seed(1)
R<-1000000

tosses<-sample(c("H","T"),R,replace=T)

gap<-NULL
freq<-NULL
par(mfrow=c(1,2))
trials<-NULL
for (r in seq(1,R,by=5000)){
  lH<-length(which(tosses[1:r]=="H"))
  lT<-length(which(tosses[1:r]=="T"))
  gap<-c(gap,abs(lH-lT))
  freq<-c(freq,lH/r)
  trials<-c(trials,r)
 
  cat(".")
  
}
cat("\n")
plot(trials,freq,type="l",ylim=c(0.2,0.6),xlab="Number of trials",
     ylab="Relative frequency")
lines(trials,0.5+numeric(length(freq)))
plot(trials,gap,type="l",xlab="Number of trials",
     ylab="Absolute difference (no. heads and tails)")
lines(trials,numeric(length(gap)))