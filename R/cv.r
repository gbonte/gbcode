
CVDemo<-function(){

  N<- 100
  n<-3

  set.seed(555)
  X <- rnorm(N*n)
  X<-array(X,c(N,n))

  Y<-X[,1]^2+5*X[,3]+4*log(abs(X[,2]))

  N.tr <- N/2

  X.tr <-X[1:N.tr,]
  Y.tr <- Y[1:N.tr]

  K<-10
  N.k<-N/K
  cv<-rep(0,K)
  if (T) {
    for (k in 1:K)
    {
      I.k.ts<-((k-1)*N.k+1):(N.k*k)
      I.k.tr<-setdiff(1:N.tr,I.k.ts)
      X.k.tr <-X[I.k.tr,]
      Y.k.tr <- Y[I.k.tr]
      set.seed(555)
      model.nn<- nnet (X.k.tr,Y.k.tr,size=25, maxit=10000,linout=T,trace=F)
      X.k.ts <-X[I.k.ts,]
      Y.k.ts <- Y[I.k.ts]
      Y.k.hat.ts <- predict(model.nn,X.k.ts)
      test.k.MSE <- mean((Y.k.ts-Y.k.hat.ts)^2)
      cv[k]<-test.k.MSE
    }
  }
  I.ts<-(N.tr+1):N
  X.ts <-X[I.ts,]
  Y.ts <- Y[I.ts]



  set.seed(555)
  model.nn<- nnet (X.tr,Y.tr,size=25, maxit=10000,linout=T,trace=T)

  Y.hat.tr <- predict(model.nn,X.tr)
  empirical.MSE <- mean((Y.tr-Y.hat.tr)^2)
  print(paste("Empirical MSE=",round(empirical.MSE,2)))
  Y.hat.ts <- predict(model.nn,X.ts)
  test.MSE <- mean((Y.ts-Y.hat.ts)^2)

  print(paste("Test MSE=",round(test.MSE,2)))


  cv.MSE <-mean(cv)
  cv.MSE

  print(paste("Cross-validation MSE=",round(cv.MSE,2)))
}
