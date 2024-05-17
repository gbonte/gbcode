## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())

R<-250 ## number of Monte Carlo repetitions

N<-500 ## size of training set
d<-3 ## degree of f (ground truth)
m<-2 ## degree of  polynomial model
Nts<-10000 ## size of test set
sdnoise=0.1 ## sdev noise

beta=c(1,-1,0.75,-0.5,0.25, -0.125, 0.06)
beta=beta[1:(d+1)]

ff<-function(X,beta,sdw=sdnoise){
  N<-NROW(X)
  n<-NCOL(X)
  XX<-cbind(numeric(N)+1,X)
  return(XX%*%beta+rnorm(N,sd=sdw))
}



MISE=NULL
MISEloo=NULL
for (r in 1:R){
  set.seed(r)
  xtr<-rnorm(N)
  Xtr<-xtr
  for (i in 2:d)
    Xtr<-cbind(Xtr,xtr^i)
  Ytr=ff(Xtr,beta)
  
  xts<-rnorm(Nts)
  Xts<-xts
  for (i in 2:d)
    Xts<-cbind(Xts,xts^i)
  Yts=ff(Xts,beta)
  
  Xtr2=Xtr[,1:(d-1)]
  XXtr2=cbind(numeric(N)+1,Xtr2)
  
  Xts2=Xts[,1:(d-1)]
  XXts2<-cbind(numeric(Nts)+1,Xts2)
  
  betahat=solve(t(XXtr2)%*%XXtr2)%*%t(XXtr2)%*%Ytr
  Yhats=XXts2%*%betahat
  e=mean((Yts-Yhats)^2)
  MISE=c(MISE,e)
  
  xtr<-rnorm(N)
  Xtr<-xtr
  for (i in 2:d)
    Xtr<-cbind(Xtr,xtr^i)
  Ytr=ff(Xtr,beta)
  eloo=NULL
  for (i in 1:N){
    Xtri=Xtr[-i,1:(d-1)]
    Xtsi=Xtr[i,1:(d-1)]
    XXtri=cbind(numeric(N-1)+1,Xtri)
    betahati=solve(t(XXtri)%*%XXtri)%*%t(XXtri)%*%Ytr[-i]
    Yhatsi=rbind(c(1,Xtsi))%*%betahati
    eloo=c(eloo,(Ytr[i]-Yhatsi)^2)
  }
  MISEloo=c(MISEloo,eloo)
}

print(mean(MISE))
print(mean(MISEloo))

MISEloo=MISEloo[MISEloo<quantile(MISEloo,0.95)]
hist(MISEloo)
abline(v=mean(MISEloo), col="red")