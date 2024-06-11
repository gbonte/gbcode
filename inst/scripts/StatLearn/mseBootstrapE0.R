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

K=1 ## number of nearest neighbors

Xts<-rnorm(Nts)
Yts<-f(Xts,sd=sdw)

### generalisation error computed with a large test set
Ets=NULL
for (i in 1:Nts){
  Yhati=pred("knn",X,Y,Xts[i],classi=FALSE,k=K)
  Ets=c(Ets,Yts[i]-Yhati)
}
MSEts=mean(Ets^2)

### empirical error 
Eemp=NULL
for (i in 1:N){
  Yhati=pred("knn",X,Y,X[i],classi=FALSE,k=K)
  Eemp=c(Eemp,Y[i]-Yhati)
}

MSEemp=mean(Eemp^2)


### E0 bootstrap assessment of the generalisation error 

Bi=numeric(N) 
## vector containing the number of bootstrap sets not containing the ith point of the training set

E0=array(NA,c(N,B))
Biasb=NULL
for (b in 1:B){
  Ib=sample(1:N,N,replace=TRUE)
  Xb=X[Ib]
  Yb=Y[Ib]
  
  for (i in 1:N){
    if (!is.element(X[i],Xb))
      Bi[i]=Bi[i]+1 ## increment of B[i] if the ith point is not in the bth bootstrap set
    Yhati=pred("knn",Xb,Yb,X[i],classi=FALSE,k=K)
    E0[i,b]=(Y[i]-Yhati)^2
    
  }
  
}
MSEbootE0=0
for (i in 1:N)
  if (Bi[i]>0)
    MSEbootE0=MSEbootE0+sum(E0[i,])/Bi[i]

MSEbootE0=MSEbootE0/N

cat("MSEts=",MSEts,"MSEemp=",MSEemp,"MSEbootE0=",MSEbootE0,
    "MSEboot.632=",0.632*MSEbootE0+0.328*MSEemp,"\n")