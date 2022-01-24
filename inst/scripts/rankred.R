# Script showing that highly correlated features (between them and with the target)
## are nevertheless informative
rm(list=ls())
set.seed(0)
N=50

pars=rnorm(4,sd=2)
Z=rnorm(N)
sdw=0.2

X1=pars[1]*Z+rnorm(N,sd=sdw)
X2=pars[2]*Z+rnorm(N,sd=sdw)

Y=pars[3]*X1+pars[4]*X2+rnorm(N,sd=0.2)

print(cor(cbind(X1,X2,Y)))

X=cbind(numeric(N)+1,X1,X2)


for (nfs in 1:2){
  
  Ntr=N/2
  
  Xtr=X[1:Ntr,1:(nfs+1)]
  Ytr=Y[1:Ntr]
  
  betatr=solve(t(Xtr)%*%Xtr)%*%t(Xtr)%*%Ytr
  ## least-squares estimation
  
  Xts=X[(Ntr+1):N,1:(nfs+1)]
  Yts=Y[(Ntr+1):N]
  
  ets=Yts-Xts%*%betatr ## test error
  
  cat("# features=",nfs, "MSE test=",mean(ets^2),"\n")
}


