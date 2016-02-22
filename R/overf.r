
Overf<-function(){
  library(nnet)
  data(rock)
  attach(rock)
  area1<- area/10000
  peri1 <- peri/1000

  N<-nrow(rock)

  N.tr <- 25

  X.tr <-cbind(area1[1:N.tr],peri1[1:N.tr],shape[1:N.tr])
  Y.tr <- log(perm[1:N.tr])

  I.ts<-(N.tr+1):N
  X.ts <-cbind(area1[I.ts],peri1[I.ts],shape[I.ts])
  Y.ts <- log(perm[I.ts])



  set.seed(555)
  model.nn<- nnet (X.tr,Y.tr,size=15, maxit=10000,linout=T,trace=T)

  Y.hat.tr <- predict(model.nn,X.tr)
  empirical.MSE <- mean((Y.tr-Y.hat.tr)^2)
  empirical.MSE
  Y.hat.ts <- predict(model.nn,X.ts)
  test.MSE <- mean((Y.ts-Y.hat.ts)^2)
  test.MSE
}
