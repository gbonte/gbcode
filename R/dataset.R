


dopler<-function(x){
  20*sqrt(x*(1-x))*sin(2*pi*1.05/(x+0.05))
}


dataset.dopler<-function(N,sigma=1){
  set.seed(0)
  x<-sort(runif(N,min=0.12,max=1))
  y<-dopler(x)+rnorm(N,sd=sigma)
  x.ts<-sort(runif(N,min=0.12,max=1))
  y.ts<-dopler(x.ts)
  list(x=x,y=y,x.ts=x.ts,y.ts=y.ts)
}


#D<-dataset.dopler(2000)

#d<-data.frame(D$y,D$x)
#names(d)<-c("Y","X")
#mod.nn<-nnet(Y~.,data=d,size=30,skip=FALSE,trace=FALSE, maxit=300,linout=TRUE)
#d.ts<-data.frame(D$y.ts,D$x.ts)
#names(d.ts)<-c("Y","X")
#p<-predict(mod.nn,d.ts)


#plot(D$x,D$y,type="l")
#plot(D$x.ts,D$y.ts,type="l")
#lines(D$x.ts,p,col="red")
