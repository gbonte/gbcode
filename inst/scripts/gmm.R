rm(list=ls())
library(mvtnorm)


## mixture of gaussians density
dmixture<-function(x1,x2, w, centers,sds){
  m=length(w)
  dens<-0
  
  for (i in 1:m)
    dens=dens+w[i]*dmvnorm(c(x1,x2),mean=centers[i,],sigma=diag(sds[i,]))
  
  return(dens)
}

## random sampling from mixture of gaussians
GMM<-function(N,n,w,centers,sds){
  ## m : number of mixture components
  m=length(w)
  X=NULL
  W=NULL ## keep strace of which component was sampled
   for (i in 1:N){
    whichm=sample(1:m,1,prob=w)
    W=c(W,whichm)
    if (n>1)
      X=rbind(X,rmvnorm(1,mean=centers[whichm,],sigma=diag(sds[m,])))
    else
      X=c(X,rnorm(1,mean=centers[whichm],sd=sds[m]))
    
  }
  
  return(list(X=X,W=W,centers=centers,sds=sds))
}
set.seed(0)
cols=rep(c("red","green","blue","magenta","black","yellow"),3)
N=2000
m=3
n=2
w=runif(m)
w=w/sum(w) 
centers=array(rnorm(m*n,sd=1.5),c(m,n))
sds=array(runif(m*n,0,0.6),c(m,n))

x1=seq(-3,3,by=.1)
x2=seq(-3,3,by=.1)

vd <- Vectorize(dmixture, c("x1", "x2"))
d <- outer(x1, x2,vd,w,centers,sds)

D=GMM(N,2,w, centers,sds)

par(mfrow=c(1,2))
persp(x1, x2, d, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      main=paste("Mixture of gaussians with ", m, "components"))

plot(D$X[,1],D$X[,2],xlab="x1",ylab="x2",
     main="Samples")

for (class in 1:m){
  w=which(D$W==class)
  points(D$X[w,1],D$X[w,2],col=cols[class+2])
}
