## Monte Carlo estimation of generalization error
rm(list=ls())

S=5000 ## number of MC trials
sdw=1 ## noise stdev
N<-50  ## number of training examples

rangeAlpha=seq(1.4,3.4,by=0.01) # set Lambda of alpha

RalphaN=NULL
Ralpha=NULL
Remp=NULL
RempDN=array(NA,c(S,length(rangeAlpha)))
AL=NULL


## testset for functional risk computation
Nts=100000
Xts<-runif(Nts,-2,2) 
Yts=Xts^3+rnorm(Nts,0,sdw) 


for (s in 1:S){
  set.seed(s)
  ## dataset generation
  
  Xtr<-runif(N,-2,2) 
  Ytr=Xtr^3+rnorm(N,0,sdw)  ## f(x)=x^3
  
  
  ### ERM parametric identification 
  bestRemp=Inf
  cnt=1
  for (alpha in rangeAlpha){
   
    RempN=mean((Ytr-alpha*Xtr)^2)
    RempDN[s,cnt]=RempN
    cnt=cnt+1
    if (RempN<bestRemp){
      bestRemp=RempN
      alphaN=alpha
    }
    
  }
  
  Remp=c(Remp,bestRemp)
  RalphaN=c(RalphaN,mean((Yts-alphaN*Xts)^2))
  AL=c(AL,alphaN)
  
}
for (alpha in rangeAlpha)
  Ralpha=c(Ralpha,mean((Yts-alpha*Xts)^2))


cat(paste("\n E[R(alpha_N)]=",mean(RalphaN)))
cat(paste("\n alpha_0=",rangeAlpha[which.min(Ralpha)]))
par(mfrow=c(1,4))
hist(RalphaN, main="Sampling distribution of R(alpha_N)")
abline(v=mean(RalphaN),col="green")
hist(Remp, main="Sampling distribution of Remp(alpha_N)")
abline(v=mean(Remp),col="red")
hist(AL, main="Sampling distribution of alpha_N",xlab="alpha_N")
plot(rangeAlpha,Ralpha,xlim=c(2,3),ylim=c(2.4,2.8),type="l",ylab='R(alpha)',xlab="alpha")
points(rangeAlpha[which.min(Ralpha)],min(Ralpha),lwd=5)
abline(h=mean(Remp),col="red",lwd=2)
abline(h=mean(RalphaN),col="green")
par(mfrow=c(1,1))
plot(rangeAlpha,Ralpha,type="l",ylab='R(alpha)',ylim=c(1.5,3.8),xlab="alpha",lwd=3)
lines(rangeAlpha,4*rangeAlpha^2/3-32/5*rangeAlpha+71/7,lwd=3,col="orange") ## analytical expression of R(\alpha)
for (s in 1:50){
  lines(rangeAlpha,RempDN[s,],lty=2)
  points(rangeAlpha[which.min(RempDN[s,])],1.5,lwd=2) 
}