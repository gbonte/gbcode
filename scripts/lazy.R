lazyExample<-function(){
  rm(list=ls())
  source("dopler.R")
  library(lazy)

  N<-2000
  D<-dataset.dopler(2000)
  plot(D$x,D$y,type="l")

  d<-data.frame(D$y,D$x)
  names(d)<-c("Y","X")

  for (number.neighbors in seq(3,3000,by=25)){

    mod.lazy<-lazy(Y~.,d,
                   control=lazy.control(conIdPar=NULL,
                                        linIdPar=c(number.neighbors,number.neighbors), quaIdPar=NULL))

    d.ts<-data.frame(D$y.ts,D$x.ts)
    names(d.ts)<-c("Y","X")
    p<-predict(mod.lazy,d.ts)



    plot(D$x.ts,D$y.ts,type="l", main=paste("Number neighbors=",number.neighbors))
    lines(D$x.ts,c(p$h),col="red")
    Sys.sleep(.1)
  }
}
