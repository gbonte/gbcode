## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

library(gbcode)

f<-function(X,sd=0.15){
  N<-length(X)
  sin(X)+rnorm(N,sd=sd)
}

N<-20 ## size training set
Nts<-10000  ## size test set

sdw=0.2 ## stdev noise
X<-rnorm(N)
B<-500. ## number bootstrap sets
Y<-f(X,sd=sdw)

K=3 ## number of nearest neighbors

Xts<-rnorm(Nts)
Yts<-f(Xts,sd=sdw)


Ets=NULL
for (i in 1:Nts){
  Yhati=pred("knn",X,Y,Xts[i],classi=FALSE,k=K)
  Ets=c(Ets,Yts[i]-Yhati)
  
}
MSEts=mean(Ets^2)


Eemp=NULL
for (i in 1:N){
  Yhati=pred("knn",X,Y,X[i],classi=FALSE,k=K)
  Eemp=c(Eemp,Y[i]-Yhati)
  
}
MSEemp=mean(Eemp^2)

Biasb=NULL
for (b in 1:B){
  Ib=sample(1:N,N,replace=TRUE)
  Xb=X[Ib]
  Yb=Y[Ib]
  Eb=NULL
  for (i in 1:N){
    Ydoti=pred("knn",Xb,Yb,Xb[i],classi=FALSE,k=K)
    Yhati=pred("knn",Xb,Yb,X[i],classi=FALSE,k=K)
    Eb=c(Eb,(Yb[i]-Ydoti)^2-(Y[i]-Yhati)^2)
    
  }
  Biasb=c(Biasb,mean(Eb))
}
Biasboot=mean(Biasb)

cat("MSEts=",MSEts,"MSEemp=",MSEemp,"Bias B=",Biasboot,
    "BiasCorrected MSE=",MSEemp-Biasboot,"\n")