
EstLS<-function(){
  par(ask=TRUE)
  X<-seq(-1,1,by=.1)

  beta0<-1
  beta1<--1
  N<-length(X)
  R<-10000
  sd.w<-0.1


  Y<-beta0+beta1*X+rnorm(N,sd=sd.w)
  x.hat<-mean(X)
  y.hat<-mean(Y)
  S.xy<-sum((X-x.hat)*Y)
  S.xx<-sum((X-x.hat)^2)

  beta.hat.1<-S.xy/S.xx
  beta.hat.0<-y.hat-beta.hat.1*x.hat


  b0 <- seq(-5, 5, length= 50)
  b1<-b0
  rss <- function(b0,b1) {
    RSS<-sum((Y-b0-b1*X)^2)
    RSS
  }


  RSS<-array(NA,c(length(b0),length(b1)))

  for (i in 1:length(b0)){
    for (j in 1:length(b1)) {
      RSS[i,j]<-rss(b0[i],b1[j])
    }
  }

  persp(b0, b1, sqrt(RSS),
        theta = 30,
        phi = 30,
        expand = 0.5,
        col = "lightblue",
        ticktype="detailed",
        main=paste("Beta.hat.0=", round(beta.hat.0,2), ", Beta.hat.1=", round(beta.hat.1,2)))

  contour(b0,b1,sqrt(RSS),
          nlevels=100)

  title(main=paste("Beta.hat.0=", round(beta.hat.0,2), ", Beta.hat.1=", round(beta.hat.1,2)),
        xlab="b0",
        ylab="b1")
}
