EEMean<-function() {
  rm(list=ls())
  par(ask=TRUE)
  N<-10
  R<-10000
  sd.w<-5

  EE<-numeric(R)
  MSE<-numeric(R)

  for (r in 1:R){
    Y<-rnorm(N,sd=sd.w)
    mu.hat.y<-mean(Y)

    EE[r]<-sum((Y-mu.hat.y)^2) #empirical error


    Yts<-rnorm(N,sd=sd.w)
    MSE[r]<-sum((Yts-mu.hat.y)^2)
  }


  EE.th<-(N-1)*sd.w^2

  print(paste("Theoretical empirical error=",EE.th, "; Observed =",
              mean(EE) ))

  MSE.th<-(N+1)*sd.w^2

  print(paste("Theoretical MISE=",MSE.th, "; Observed =",
              mean(MSE) ))
}
