
## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

#################################################################
# bv.R					     		        #
#################################################################
## Dataset D_N={x_i,y_i} :			
#	  y_i = beta_0 + beta_1x_i + w_i			#
#   with known  beta_0  and beta_1 and known w=Normal(0, sigma) 	#
#   						
## Visualization of bias and variance of the least square estimate 	#
#################################################################



# preliminary
# =============
rm(list=ls())
par(ask=FALSE)
par(mfrow=c(1,1))
set.seed(0)
X<-seq(-10,10,by=0.25) 	# fixed xi
beta0<--1 	
beta1<-1
sd.w<-3
N<-length(X)
R<-50 		# number of MC trials


beta.hat.1<-numeric(R)
beta.hat.0<-numeric(R)
var.hat.w<-numeric(R)
Y.hat<-array(NA,c(R,N))
x.bar<-mean(X)
S.xx<-sum((X-x.bar)^2)



for (r in 1:R){
  Y<-beta0+beta1*X+rnorm(N,sd=sd.w) ## data generation
  y.bar<-mean(Y)
  S.xy<-sum((X-x.bar)*Y)
  
  beta.hat.1[r]<-S.xy/S.xx
  beta.hat.0[r]<-y.bar-beta.hat.1[r]*x.bar
  
  Y.hat[r,]<-beta.hat.0[r]+beta.hat.1[r]*X
  var.hat.w[r]<-sum((Y-Y.hat[r,])^2)/(N-2)
  plot(X,beta0+beta1*X,type="l",col="green",
       main=paste("beta0=",beta0," beta1=",beta1,"sdw=", sd.w,"  N=",N),
       ylim=c(-10,10),lwd=3,ylab="y")
  points(X,Y)
  for (j in 1:r)
    lines(X,Y.hat[j,],col="grey")
  lines(X,beta0+beta1*X,type="l",col="green",lwd=5)
  #browser()
}


lines(X,apply(Y.hat,2,mean),col="red",lwd=3)

