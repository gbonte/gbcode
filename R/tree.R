treeExample<-function(){
  rm(list=ls())
  source("dopler.R")
  library(tree)

  N<-2000
  D<-dataset.dopler(2000)
  plot(D$x,D$y,type="l")

  d<-data.frame(D$y,D$x)
  names(d)<-c("Y","X")

  for (number.nodes in 1:30){

    mod.tree<-tree(Y~.,d,minsize=number.nodes,mindev=0)
    d.ts<-data.frame(D$y.ts,D$x.ts)
    names(d.ts)<-c("Y","X")
    p<-predict(mod.tree,d.ts)



    plot(D$x.ts,D$y.ts,type="l", main=paste("Min number points per leaf=",number.nodes))
    lines(D$x.ts,p,col="red")
    browser()
  }
}
