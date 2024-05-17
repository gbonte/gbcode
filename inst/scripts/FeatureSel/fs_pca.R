## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
set.seed(0)
library(gbcode)
N=50


# correlated inputs
X1=rnorm(N,sd=3)
X2=3*X1+rnorm(N,sd=0.4)
X3=0.2*X2+rnorm(N,sd=0.5)
X4=-X3+rnorm(N,sd=0.5)

W=rnorm(N,sd=0.5)
X=cbind(array(rnorm(N*5),c(N,5)),X1,X2,X3,X4)

Y=scale(X1^2+2*X2*X3+X4)+W

n=NCOL(X)
E<-array(NA,c(N,n))
EPCA<-array(NA,c(N,n))

## LEAVE-ONE-OUT loop
for (i in 1:N){
  
  Xtr=scale(X[-i,])
  Ytr=Y[-i]
  
  ## normalization of the input test
  Xts=(X[i,]-attr(Xtr,"scaled:center"))/attr(Xtr,"scaled:scale")
  
  S=svd(Xtr/sqrt(N-1))
  ## PC loop
  for (h in 1:n){
    V=(S$v)
    Vh=array(V[,1:h],c(n,h))
    Zh=Xtr%*%Vh
    
    Zi=Xts%*%Vh
    YhatPCAi=pred("knn",Zh,Ytr,Zi,class=FALSE)
    Yhati=pred("knn",Xtr[,1:h],Ytr,Xts[1:h],class=FALSE)
    
    
    EPCA[i,h]=(Y[i]-YhatPCAi)^2
    E[i,h]=(Y[i]-Yhati)^2
  }
  

}

cat("MSEloo PCA=",apply(EPCA,2,mean),"\n best PC number=",which.min(apply(EPCA,2,mean)),"\n")
cat("MSEloo =",apply(E,2,mean),"\n")


X=scale(X)
S=svd(X)
Z=X%*%(S$v)

cat("\n --- \n Variance Z=",apply(Z,2,var), "\n Eigenvalues=",S$d^2/(N-1),"\n")

plot(S$d^2/(N-1),type="l",ylab="Eigenvalues")

