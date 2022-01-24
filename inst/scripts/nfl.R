## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

set.seed(0)
F=500
N=100
Nts=50000

ehat1=NULL
ehat2=NULL
ehat3=NULL
ehat4=NULL
ehat5=NULL
ehat6=NULL

m1=2
m2=3
M=5 ## max degree order 
for (f in 1:F){
  
  X<-rnorm(N)
  Xts=rnorm(Nts)
  sdw=runif(1,0.01,0.5)
  m=sample(1:M ,1) ## random target order
  beta=rnorm(m+1,sd=0.5) ## random target parameters
  
  DX=numeric(N)+1
  DXts=numeric(Nts)+1
  for (j in 1:M){
    DX=cbind(DX,X^j)  ## input training
    DXts=cbind(DXts,Xts^j) 
  }
  
  DX0=DX[,1:(m+1)]
  DXts0=DXts[,1:(m+1)]
  Y=DX0%*%beta+rnorm(N,sd=sdw)
  Yts=DXts0%*%beta+rnorm(Nts,sd=sdw)
  
  ###################################################
  ################################################@@
  ## Degree m1: model fitting
  DXm=DX[,1:m1]
  DXtsm=DXts[,1:m1]
  betahat=solve(t(DXm)%*%DXm)%*%t(DXm)%*%Y
  Yhatts=DXtsm%*%betahat
  
  ## Degree m2: model fitting
  DXm=DX[,1:m2]
  DXtsm=DXts[,1:m2]
  betahat=solve(t(DXm)%*%DXm)%*%t(DXm)%*%Y
  Yhatts2=DXtsm%*%betahat
  
  ## train and test model selection between degree m1 and m2
  Ntr=round(N/2)
  Itr=sample(1:N,Ntr)
  Its=setdiff(1:N,Itr)
  DXm=DX[Itr,1:m1]
  DXvm=DX[Its,1:m1]
  betahat=solve(t(DXm)%*%DXm)%*%t(DXm)%*%Y[Itr]
  Eval=Y[Its]-DXvm%*%betahat ## validation error model degree m1
  
  DXm2=DX[Itr,1:m2]
  DXvm2=DX[Its,1:m2]
  betahat2=solve(t(DXm2)%*%DXm2)%*%t(DXm2)%*%Y[Itr]
  Eval2=Y[Its]-DXvm2%*%betahat2 ## validation error model degree m1
  
  
  ### model selection (select the best)
  if (mean(Eval^2)< mean(Eval2^2)){
    DXm=DX[,1:m1]
    betahat=solve(t(DXm)%*%DXm)%*%t(DXm)%*%Y
    DXtsm=DXts[,1:m1]
    Yhatts3=DXtsm%*%betahat
  } else{
    DXm=DX[,1:m2]
    betahat2=solve(t(DXm)%*%DXm)%*%t(DXm)%*%Y
    DXtsm=DXts[,1:m2]
    Yhatts3=DXtsm%*%betahat2
  }
  
  ####  select the worst
  
  if (mean(Eval^2)> mean(Eval2^2)){
    DXm=DX[,1:m1]
    betahat=solve(t(DXm)%*%DXm)%*%t(DXm)%*%Y
    DXtsm=DXts[,1:m1]
    Yhatts4=DXtsm%*%betahat
  } else{
    DXm=DX[,1:m2]
    betahat2=solve(t(DXm)%*%DXm)%*%t(DXm)%*%Y
    DXtsm=DXts[,1:m2]
    Yhatts4=DXtsm%*%betahat2
  }
  
  
  ### random fitting
  Yhatts5=rnorm(Nts)
  
  ### random fitting 2
  Yhatts6=runif(Nts,-2,2)
  
  ehat1=c(ehat1,mean((Yts-Yhatts)^2)/var(Yts))
  ehat2=c(ehat2,mean((Yts-Yhatts2)^2)/var(Yts))
  ehat3=c(ehat3,mean((Yts-Yhatts3)^2)/var(Yts))
  ehat4=c(ehat4,mean((Yts-Yhatts4)^2)/var(Yts))
  ehat5=c(ehat5,mean((Yts-Yhatts5)^2)/var(Yts))
  ehat6=c(ehat6,mean((Yts-Yhatts6)^2)/var(Yts))
  
  cat("f=",f, " E1=",median(ehat1), "E2=", median(ehat2), "E3=",median(ehat3),
      "E4=",median(ehat4),"E5=",median(ehat5),"E6=",median(ehat6),"\n")
  
}

E=cbind(ehat1,ehat2,ehat3,ehat4,ehat5,ehat6)
colnames(E)<-c("A1","A2","selbest","selworst","rand1","rand2")
boxplot(E,ylim=c(0,8),ylab="NMSE")
          