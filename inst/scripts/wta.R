## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())

algo1<-function(Xtr,Ytr,Xts){
  betahat=solve(t(Xtr)%*%Xtr)%*%t(Xtr)%*%Ytr
  Yhatts=Xts%*%betahat
} 


wta<-function(Xtr,Ytr,Xts,win=TRUE){
  N=NROW(Xtr)
  Ntr=round(N/2)
  
  Itr=sample(1:N,Ntr)
  Its=setdiff(1:N,Itr)
  DXm=Xtr[Itr,1:m1]
  DXvm=Xtr[Its,1:m1]
  
  Yhatv=algo1(DXm, Ytr[Itr],DXvm)
  Eval=Y[Its]-Yhatv ## validation error model degree m1
  Yhatv=algo1(DXvm, Ytr[Its],DXm)
  Eval=c(Eval,Y[Itr]-Yhatv) 
  
  Yhats=algo1(Xtr[,1:m1], Ytr,Xts[,1:m1])
  
  
  DXm2=DX[Itr,1:m2]
  DXvm2=DX[Its,1:m2]
  Yhatv2=algo1(DXm2, Ytr[Itr],DXvm2)
  Eval2=Y[Its]-Yhatv2 ## validation error model degree m1
  Yhatv2=algo1(DXvm, Ytr[Its],DXm)
  Eval2=c(Eval2,Y[Itr]-Yhatv2) 
  
  Yhats2=algo1(Xtr[,1:m2], Ytr,Xts[,1:m2])
  
  
  if (win){
    if (mean(abs(Eval))< mean(abs(Eval2)))
      return(Yhats)
    else
      return(Yhats2)
  }
  if (!win){
    if (mean(abs(Eval))> mean(abs(Eval2)))
      return(Yhats)
    else
      return(Yhats2)
  }
}


N=80
Nts=50000

ehat1=NULL
ehat2=NULL
ehat3=NULL
ehat4=NULL
ehat5=NULL
ehat6=NULL

set.seed(0)
m1=5
m2=2

M=8 ## max degree order 
R=100

Xts=rnorm(Nts)



#for (sdw in seq(0.1,10,by=0.5)){
sdw=0.1
for (M in seq(max(m1,m2),20,by=1)){
  beta=rnorm(M+1,sd=1) ## random target parameters
  for (r in 1:R){
    X<-rnorm(N)
    DX=numeric(N)+1
    DXts=numeric(Nts)+1
    for (j in 1:M){
      DX=cbind(DX,X^j)  ## input training
      DXts=cbind(DXts,Xts^j) 
    }
    Y=DX%*%beta+rnorm(N,sd=sdw)
    Yts=DXts%*%beta+rnorm(Nts,sd=sdw)
    
    ###################################################
    ################################################@@
    ## Degree m1: model fitting
    DXm=DX[,1:m1]
    DXtsm=DXts[,1:m1]
    
    Yhatts=algo1(DXm,Y,DXtsm)
    
    ## Degree m2: model fitting
    DXm=DX[,1:m2]
    DXtsm=DXts[,1:m2]
    Yhatts2=algo1(DXm,Y,DXtsm)
    
    ## train and test model selection between degree m1 and m2
    
    Yhatts3=wta(DX,Y,DXts)
    Yhatts4=wta(DX,Y,DXts,FALSE)
    
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
    
    
    
  }
  cat("r=",r, " E1=",median(ehat1), "E2=", median(ehat2), "Ewta=",median(ehat3),
      "Elta=",median(ehat4),"E5=",median(ehat5),"E6=",median(ehat6),"\n")
  
  E=cbind(ehat1,ehat2,ehat3,ehat4,ehat5,ehat6)
  colnames(E)<-c(paste("m=",m1),paste("m=",m2),"selbest","selworst","rand1","rand2")
  boxplot(E,ylim=c(0,2),ylab="NMSE",main=paste("sdw=",sdw,"M=",M, "N=",N))
}

eta=0.2
V=eta
B=seq(0,10,by=0.1)
plot(B,(B+eta+V)/(2*(eta+V)*(eta+V+2*B)))


mu=-1
sigma=0.41
N=10000
z=rnorm(N,mu,sigma)
mean(z^2)
var(z^2)
2*sigma^2*(sigma^2+2*mu^2)




mean((theta-thetahat)^2)
var((theta-thetahat)^2)

2*sigma^2*(sigma^2+2*B^2)

var((theta-thetahat)^2)
var(abs(theta-thetahat))