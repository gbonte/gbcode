rm(list=ls())
set.seed(1)
par(mfrow=c(1,2))
N=40. ## number of examples
n=20 ## number of features

X=array(rnorm(N*n),c(N,n))
## original data set

S=svd(X)
# u: matrix whose columns contain the left singular vectors of x, present if nu > 0. Dimension c(N, min(N, n)).
#d: vector containing the singular values of x, of length min(N, n), sorted decreasingly.
# v: matrix whose columns contain the right singular vectors of x, present if nv > 0. Dimension c(n, min(N, n)).

XX=X*0


## X= \sum_{j=1}^min(N,n) \sigma_j U_j V_j^T 
## where U_j is the jth column of U and V_j is the jth column of V (or the jth row of V^T) 
for (j in 1:min(N,n))
  XX=XX+S$d[j]*S$u[,j]%*%rbind(S$v[,j])


cat("Error reconstructed with", min(N,n), "components",
    mean((X-XX)^2),"\n") # sum(sqrt(S$d[(k+1):length(S$d)])),"\n")


RecErr<-NULL
NormErr<-NULL
for (k in 1:min(N,n)){
  ## approximation with k components
  cat("\n--------\n")
  XX=X*0
  
  for (j in 1:k)
    XX=XX+S$d[j]*S$u[,j]%*%rbind(S$v[,j])
  
  
  ## check of relation above
  cat("Error reconstructed with", k, "components",norm((X-XX),"F"),"\n") # sum(sqrt(S$d[(k+1):length(S$d)])),"\n")
  if (k>1){
    Z=S$u[,1:k]%*%diag(S$d[1:k])
    ## [N,k] latent representation
    
    Xr=Z%*%t(S$v[,1:k])
    ## reconstructed matrix [N,m]
    
    cat("Error reconstructed with", k, "components",norm((X-XX),"F"),"\n")
  }
  RecErr<-c(RecErr,norm((X-XX),"F"))
  NormErr<-c(NormErr,norm((X-XX),"2"))
}

plot(1:min(N,n),RecErr,type="l",
     main="Rank-k approximation",
     xlab="# components", ylab="Frobenius norm of error")

plot(1:min(N,n),NormErr,type="l",
     main="Rank-k approximation",
     xlab="# components", ylab="Spectral norm of error")
points(c(S$d[2:length(S$d)]),col="red")