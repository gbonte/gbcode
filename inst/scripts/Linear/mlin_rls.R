## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
par(ask=TRUE)

n<-10

N<-100
X<-array(rnorm(N*n),c(N,n))
Y<-cbind(X[,1]+X[,n]+rnorm(N,sd=0.1),X[,2],-5*X[,3]+rnorm(N,sd=1))
m<-NCOL(Y)

rls<-function(x,y,t,P,mu=1){
  x=rbind(x)
  P.new <-(P-(P%*%t(x)%*%x%*%P)/as.numeric(1+x%*%P%*%t(x)))/mu
  ga <- P.new%*%t(x)
  
  epsi <- y-x%*%t
  
  t.new<-t+ga%*%as.numeric(epsi)
  list(t.new,P.new,mean(epsi^2))
}


N<-NROW(X)



t<-array(0,c(n+1,m))
P<-500*diag(n+1)
mu<-0.95
E<-NULL
for (i in 1:N){
  rls.step<-rls(c(1, X[i,]),Y[i,],t,P,mu)
  t<-rls.step[[1]]
  P<-rls.step[[2]]
  E<-c(E,rls.step[[3]])
 
}
