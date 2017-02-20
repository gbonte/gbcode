

CumDis<-function(){
  library(stepfun)

  DN<-c(20, 21, 22, 20, 23, 25, 26, 25, 20, 23, 24, 25,  26, 29)

  plot.ecdf(DN,verticals=TRUE,main="Empirical Distribution function")

}
