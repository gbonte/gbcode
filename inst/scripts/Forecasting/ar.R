## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
library(signal)                  

set.seed(0)

q=4  ## order of AR(q)
N=q*500

alpha=rnorm(q)

## stationarity check
while (any(abs(roots(c(1, -alpha)))>1))
  alpha=rnorm(q)

Y=rnorm(q)
for (i in (q+1):N){
  L=length(Y)
  Y=c(Y,sum(alpha*rev(Y[(L-q+1):(L)])+rnorm(1,sd=0.1)))
}
Y=Y[(q+1):length(Y)]


N=length(Y)
Q=20

hatacf=numeric(Q)
## Estimation auto-correlation function

for (k in 1:(Q)){
  Yk=c(numeric(k), Y)
  
  C=cor(Y[(k+1):N],Yk[(k+1):N])
  hatacf[k]=C
 
}

YY=array(Y,c(N,1))
for (k in 1:(Q+1))
  YY=cbind(YY,c(Y[(k+1):N],numeric(k)+NA))

## Estimation partial auto-correlation function by linear regression
hatpacf=NULL
for (k in 2:(Q+1))
  hatpacf=c(hatpacf,lm(YY[,1]~YY[,2:k])$coefficients[k])


par(mfrow=c(3,1), mai = 0.1*c(1,1,1,1),
    mar = 2*c(1,1,1,1))
plot(Y,xlab='',main=paste("AR(",q,")"))

plot(1:Q,hatacf,type="l",lty=1,ylab='',xlab='k',main="Est acf")
lines(1:Q,2/sqrt(N)*(numeric(Q)+1),lty=2)
legend("bottomright",c('Estimated autocor', 'thr'),lty=c(1,2),cex=0.5)

plot(1:Q,hatpacf,type="l",lty=1,ylab='',xlab='k',main="Est pacf")
lines(1:Q,2/sqrt(N)*(numeric(Q)+1),lty=2)
legend("bottomright",c('Estimated pcor', 'thr'),lty=c(1,2),cex=0.5)


print(c(acf(Y,plot=FALSE)$acf)[2:(Q-1)])
cat("\n ")

print(c(pacf(Y,plot=FALSE)$acf)[1:(Q-1)])
cat("\n ")

par(mfrow=c(3,1), mai = 0.1*c(1,1,1,1),
    mar = 2*c(1,1,1,1))
plot(Y,xlab='',main=paste("AR(",q,")"))
acf(Y)
pacf(Y)