
BaggingDemo<-function(N=200,n=3){

  set.seed(555)
  X <- rnorm(N*n)
  X<-array(X,c(N,n))

  Y<-X[,1]^2+5*X[,3]+4*log(abs(X[,2]))+0.5*rnorm(N)
  N.tr <- N/2
  I.ts<-(N.tr+1):N
  data.train<-data.frame(y=Y[1:N.tr],x1=X[1:N.tr,1],x2=X[1:N.tr,2],x3=X[1:N.tr,3])

  data.test<-data.frame(y=Y[I.ts],x1=X[I.ts,1],x2=X[I.ts,2],
                        x3=X[I.ts,3])


  ################################## Single NNET
  set.seed(555)
  model.nn<- nnet (y ~ x1+x2+x3,data.train, size=25, maxit=10000,linout=T,trace=F)
  predict.nn.1<-predict(model.nn,data.test)
  MSE.nn1<-mean((data.test$y-predict.nn.1)^2)
  print(paste("Test error single NNET=",MSE.nn1))

  #################################


  B<-50
  predict.nn<-array(0,c(length(I.ts),B))
  predict.bag<-array(0,c(length(I.ts),1))
  MSE.nn<-array(0,c(B,1))
  for (b in 1:B)
  {
    set.seed(b)
    I.D.b<-sample(seq(1,N.tr),replace=TRUE)
    data.bagging<-data.frame(y=Y[I.D.b],x1=X[I.D.b,1],x2=X[I.D.b,2],x3=X[I.D.b,3])
    set.seed(555)
    model.nn<- nnet (y ~ x1+x2+x3,data.bagging,
                     size=25, maxit=10000,linout=T,trace=F)
    predict.nn[,b]<-predict(model.nn,data.test)
    MSE.nn[b]<-mean((data.test$y-predict.nn[,b])^2)
  }


  for (i in 1:length(I.ts))
  {
    predict.bag[i]<- mean(predict.nn[i,]);
  }


  MSE.bag<- mean((data.test$y-predict.bag)^2)
  print(paste("Test error bagging NNET=",MSE.bag))


  print(paste("Average test error bagging NNET=", mean(MSE.nn)))

  hist(MSE.nn,main='')
  abline(v=MSE.bag)
}
