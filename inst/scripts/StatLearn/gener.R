## Monte Carlo estimation of generalization error
rm(list=ls())

S=1000
sdw=1

R=NULL
AL=NULL
for (s in 1:S){
  N<-100 
  Xtr<-runif(N,-2,2) 
  Ytr=Xtr^3+rnorm(N,0,sdw) 
  
  
  ### parametric identification 
  bestRemp=Inf
  for (alpha in seq(1,3,by=0.01)){
    Remp=mean((Ytr-alpha*Xtr)^2)
    if (Remp<bestRemp){
      bestRemp=Remp
      alphaN=alpha
    }
    
  }
  
  Nts=1000000
  Xts<-runif(Nts,-2,2) 
  Yts=Xts^3+rnorm(Nts,0,sdw) 
  R=c(R,mean((Yts-alphaN*Xts)^2))
  AL=c(AL,alphaN)
  
}

cat(paste("\n E[R(alpha_N)]=",mean(R)))

hist(AL, main="Sampling distribution of alpha",xlab="alpha")