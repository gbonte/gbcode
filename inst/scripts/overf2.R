Overf2<-function(){

  rm(list=ls())
  rea<-TRUE

  f<-function(x,ord){
    f<-1
    for (i in 1:ord)
      f<-f+(x^i)

    f
  }


  set.seed(1)

  n<-1
  N<-25

  x<-seq(-2,2,length.out=N)
  N<-length(x)
  sd.w<-1
  O<-3
  Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
  data.tr<-cbind(Y,x)


  R<-20

  Remp<-numeric(R)
  FPE<-numeric(R)
  PSE<-numeric(R)
  MSE.loo<-numeric(R)
  x.ts<-seq(-2,2,length.out=200)





  for (r in 1:R){
    X.ts<-NULL
    X<-NULL
    for (ord in 1:r){
      X<-cbind(X,x^ord)
      X.ts<-cbind(X.ts,x.ts^ord)
    }
    p<-r+1
    DN<-data.frame(cbind(Y,X))

    mod<-lm(Y~.,DN)
    sd.w.hat<-sqrt(sum(mod$residuals^2)/(N-p))
    Remp[r]<-sqrt(mean(mod$residuals^2))


    e.hat.i<-numeric(N)
    if (FALSE)
      for ( i in 1:N){
        DN.i<-DN[-i,]
        mod.i<-lm(Y~.,DN.i)
        e.hat.i[i]<-Y[i]-predict(mod.i,DN[i,])
      }
    MSE.loo[r]<-mean(e.hat.i^2)


    Y.ts<-f(x.ts,ord=O)
    data.ts<-data.frame(cbind(Y.ts,X.ts))
    names(data.ts)<-names(DN)
    if (rea)
      plot(x.ts,Y.ts,type="l",ylim=c(min(Y),max(Y)))
    else
      plot(x,Y)
    points(x,Y)
    lines(x.ts,predict(mod,data.ts),col="red")
    title(paste("Training error=",round(Remp[r],2), " degree=",r))
    cat(r,"\n")
    par(ask=TRUE)
  }

}
