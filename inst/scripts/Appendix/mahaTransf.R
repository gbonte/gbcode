## Mahalanobis whitening transformation
rm(list=ls())
N=500
n=50
A=array(rnorm(n*n),c(n,n))
X=array(rnorm(N*n),c(N,n))%*%A+array(rnorm(N*n,sd=0.1),c(N,n))

whiteMaha<-function(X){
  N=NROW(X)
  n=NCOL(X)
  meanX=array(apply(X,2,mean),c(1,n))
  SigmaX=cov(X)
  S=svd(SigmaX)
  W=S$u%*%diag(1/sqrt(S$d))%*%t(S$v)
  ## whitening matrix W= \Sigma^{-1/2}
  ## satisfying W^T *W =\Sigma^{-1}
  ##
  IW=S$u%*%diag(sqrt(S$d))%*%t(S$v)
  
  #W=diag(1/sqrt(S$d))%*%t(S$v)
  #IW=S$v%*%diag(sqrt(S$d))
  
  oneN=array(1,c(N,1))
  Y=(X-oneN%*%meanX)%*%W
  list(sX=Y,mX=meanX,Isigma=IW)
}

colourMaha<-function(Y,meanX,IsigmaX){
  ## colouring transformation
  N=NROW(Y)
  n=NCOL(Y)
  meanX=array(meanX,c(1,n))
  oneN=array(1,c(N,1))
 
  return(Y%*%IsigmaX+oneN%*%meanX)
}
#library("car")
S=whiteMaha(X)
C=cor(S$sX)
#scatterplotMatrix(S$sX[,1:10])
diag(C)=0
print(mean(abs(C)))
XX=colourMaha(array(S$sX[1,],c(1,NCOL(S$sX))),S$mX,S$Isigma)

cat("reconstruction error=",sum(abs(X-XX)),"\n")

#plot(X[,n],XX[,n])


