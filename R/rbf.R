rbfExample<-function(){
  rm(list=ls())
  source("dopler.R")
  library(neural)

  N<-2000
  D<-dataset.dopler(2000)
  plot(D$x,D$y,type="l")

  d<-data.frame(D$y,D$x)
  names(d)<-c("Y","X")

  for (number.neurons in c(3,20,40)){

    data<-rbftrain(array(D$x,c(N,1)),number.neurons,array(D$y,c(N,1)),
                   sigma=NaN,visual=F)
    p<-rbf(array(D$x.ts,c(N,1)),data$weigth,data$dist,data$neurons,data$sigma)





    plot(D$x.ts,D$y.ts,type="l", main=paste("Number operating regions=",number.neurons))
    lines(D$x.ts,c(p),col="red")
    browser()
  }
}
