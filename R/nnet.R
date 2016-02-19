nnetExample<-function(){
  rm(list=ls())
  source("dopler.R")
  library(nnet)


  D<-dataset.dopler(2000)
  plot(D$x,D$y,type="l")

  d<-data.frame(D$y,D$x)
  names(d)<-c("Y","X")

  for (number.nodes in 1:30){

    mod.nn<-nnet(Y~.,data=d,size=number.nodes,skip=FALSE,
                 trace=T, maxit=3000,linout=TRUE,rang=0.2)
    d.ts<-data.frame(D$y.ts,D$x.ts)
    names(d.ts)<-c("Y","X")
    p<-predict(mod.nn,d.ts)



    plot(D$x.ts,D$y.ts,type="l", main=paste("Number hidden nodes=",number.nodes))
    lines(D$x.ts,p,col="red")
    browser()
  }

}
