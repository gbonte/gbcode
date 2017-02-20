# NonLinear model (Neural network)
# Nonlinear function

BiasVarNN<-function(){

  rm(list=ls())
  library(nnet)
  par(ask=TRUE)
  f<-function(x,sd=0){
    y<-x^3+sd*rnorm(length(x))
    y
  }

  set.seed(0)
  X<-seq(-2,2,by=.05)
  N<-length(X)
  R<-100
  Y.hat<-array(NA,c(R,N))
  max.n.par<-25
  min.n.par<-3
  avg.var<-numeric(max.n.par)
  BIAS<-numeric(max.n.par)
  for (n.par in min.n.par:max.n.par){
    for (r in 1:R){
      Y<-f(X,1)
      model.nn<- nnet (X,Y,
                       size=n.par,
                       maxit=1000,
                       trace=FALSE,linout=TRUE)


      Y.hat[r,]<- predict(model.nn,array(X,c(N,1)))
      # plot(X,Y.hat[r,])
    }

    avg.var[n.par]<-round(mean(apply(Y.hat,2,"var")),2)
    regr<-apply(Y.hat,2,"mean")
    BIAS[n.par]<-round(sum((f(X)-regr)^2),2)

    if (T){
      plot(X,Y.hat[1,],
           type="l",
           main=paste("Var:",avg.var[n.par] ,"Bias:",BIAS[n.par], " n. par=",n.par),
           ylim=c(-8,8))
      for (r in 2:R){
        lines(X,Y.hat[r,])
      }
      lines(X,f(X),col="red")
      lines(X,regr,col="green")
    }
    print("BIAS=")
    print(BIAS[min.n.par:n.par])
    print("VAR=")
    print(avg.var[min.n.par:n.par])
  }


}
