# TP Modeles stochastiques II
# central.R
# Script: visualizes the distribution of the estimator
# of the variance of a non gaussian random variableshows the central limit theorem

central<-function(){
  par(ask=TRUE)
  N<-1
  R<-1000

  I<-seq(-50,50,by=.5)

  Mn<--10
  Mx<-10
  var.th<-(1/(Mx-Mn))*((Mx^3)/3-(Mn^3)/3)
  p<-dunif(I,min=Mn,max=Mx)
  plot(I,p,type="l",
       main=paste("Distribution of  r.v. z: var=",round(var.th,digits=1)))

  aver<-rep(0,R)

  for (N in 2:1000){
    for (i in 1:N){
      aver<-aver+runif(R,min=Mn,max=Mx)
    }
    aver<-aver/N
    hist(aver,freq=FALSE, main= paste("Average of N=",N, " r.v.s"),xlim=c(Mn,Mx)) #,xlim=range(I2))
    I2<-seq(-5*sd(aver),5*sd(aver),by=.5)

    p.var.hat<-dnorm(I2,mean=0,sd=sqrt(var.th/N))
    lines(I2,p.var.hat,type="l")
  }
}




