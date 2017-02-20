# TP Modeles stochastiques II
# sam_dis.R
# Script: visualizes the distribution of the estimator
# of the mean of a gaussian random variable


                                        

par(ask=TRUE)
for (N in seq(20,100,by=10)){
  mu<-0
  sdev<-10
  R<-10000
  
  I<-seq(-50,50,by=.5)
  p<-dnorm(I,mean=mu,sd=sdev)
  plot(I,p,type="l",
       main=paste("Distribution of  r.v. z: var=",sdev^2))

  mu.hat<-array(0,dim=c(R,1))
  for (r in 1:R){

    D<-rnorm(N,mean=mu,sd=sdev)

    mu.hat[r,1]<-mean(D)
  }

  hist(mu.hat,freq=FALSE, main= paste("Mean estimator on samples of size N=",N, ": var=",var(mu.hat)),xlim=c(min(I),max(I)))
  p.mu.hat<-dnorm(I,mean=mean(mu.hat),sd=sqrt(var(mu.hat)))
  lines(I,p.mu.hat,type="l")
  
  var(mu.hat)
  sdev^2/N
}
