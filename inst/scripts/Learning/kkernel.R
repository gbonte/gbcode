library(gbcode)

kern.pred<-function(X,Y,X.ts,press=FALSE,...){
  
  require(kernlab)
  N=NROW(X)
  Nts=NROW(X.ts)
  bestMSEloo=Inf
  for (lambda in seq(0.1,1,by=0.25)){
    for (s in seq(0.001,0.2,by=0.001)){
      if (!press){
        Itr=sample(1:N,round(N/2))
        Its=setdiff(1:N,Itr)
        Xtr=X[Itr,]
        Ytr=Y[Itr]
        Xcv=X[Its,]
        Ycv=Y[Its]
        Ntr=length(Itr)
      } else {
        Xtr=X
        Ytr=Y
        Ntr=N
      }
      
      ke <- rbfdot(sigma =s)
      ## calculate kernel matrix
      
      K=kernelMatrix(ke, Xtr)
      alpha=ginv(K+lambda*diag(Ntr))%*%Ytr
      
      if (press){
        Yhatr=K%*%alpha
        H=diag(K%*%ginv(t(K)%*%K+lambda*diag(Ntr))%*%t(K))
        e=((Ytr-Yhatr)/(1-H))^2
      } else {
        Kcv=kernelMatrix(ke, Xcv,Xtr)
        Yhatcv=Kcv%*%alpha
        e=(Ycv-Yhatcv)^2
      }
      MSEloo=mean(e)
      
      if (MSEloo<bestMSEloo){
        bestMSEloo=MSEloo
        bestsigma=s
        bestlambda=lambda
        print(bestMSEloo)
      }
      
    }
  }
  
  kern <- rbfdot(sigma =bestsigma)
  K=kernelMatrix(kern, X)
  Kts=kernelMatrix(kern, array(X.ts,c(Nts,NCOL(X))),X)
  alpha=ginv(K+bestlambda*diag(N))%*%Y
  Yhat=Kts%*%alpha  
  return(Yhat)
}

for (r in 1:10){
  set.seed(r)
  N=1000
  n=50
  Nts=500
  X=array(rnorm(N*n),c(N,n))
  
  
  Y=X[,1]+log(abs(X[,2]))+X[,3]^2+X[,4]*X[,5]+rnorm(N,sd=0.2)
  fs=mrmr(X[1:Nts,],Y[1:Nts],nmax=10)
  ##Yhat0=kern.pred(X[1:Nts,fs],Y[1:Nts],X[(Nts+1):N,fs],press=TRUE)
  Yhat=kern.pred(X[1:Nts,fs],Y[1:Nts],X[(Nts+1):N,fs],press=FALSE)
  Yhat0=Yhat
  Yhat2=pred("rf",X[1:Nts,],Y[1:Nts],X[(Nts+1):N,],classi=FALSE)
  Yts=Y[(Nts+1):N]
  cat("KK0=",mean((Yts-Yhat0)^2)/var(Yts),
      "KK=",mean((Yts-Yhat)^2)/var(Yts), 
      "RF=" ,mean((Yts-Yhat2)^2)/var(Yts),"\n-\n")
}