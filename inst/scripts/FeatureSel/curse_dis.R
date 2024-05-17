## Expected Euclidean distance of Standard multivariate points 
# for increasing dimension n 

rm(list=ls())
N=10000
Ed=NULL
nn=seq(1,50,by=1)


for (n in nn){
  xx<-rnorm(n)
  x0<-xx[1:n]
  nx0<-x0/sqrt(sum(x0^2))
  X=array(rnorm(N*n),c(N,n))
  D=X%*%nx0 ##
  d0=as.numeric(x0%*%nx0)
  hist(D,main=paste("n=",n,
                    "pv=",length(which(abs(D)>=abs(d0)))/N))
  abline(v=d0)
  Ed=c(Ed,mean(sqrt(apply(X^2,1,sum))))
  browser()
}

plot(nn,Ed,type="l",xlab="n",
     ylab="Expected average Euclidean distance")

lines(sqrt(nn-1/2),col="red")