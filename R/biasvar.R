
#### BiasVar ####
#' Demo of bias/variance tradeoff
#'
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Demo of bias/variance tradeoff
#'@export
#'
#'@param N: number of observations
#'@param n: max degree of polynomial fitting
#'@param R: number of repetitions for bias and variance estimation
#'@param seed: seed random generator
#'
#'@return: list with
#'\itemize{
#'\item{Remp}: training MSE
#'\item{B}: estimated squared bias
#'\item{V}: estimated sqaured variance
#'}
#'@examples
#'
#'BiasVar()
#'
#'
#'
BiasVar<-function(N=25,n=10, R=200,seed=1){


  f<-function(x,ord){
    f<-1
    for (i in 1:ord)
      f<-f+(x^i)
    f
  }

  B<-NULL
  V=NULL
  x<-seq(-2,2,length.out=N)
  N<-length(x)
  sd.w<-0.5
  O<-3
  Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
  data.tr<-cbind(Y,x)


  Remp<-numeric(n)

  x.ts<-seq(-2,2,length.out=200)


  for (r in 1:n){
    X.ts<-NULL
    X<-NULL
    for (ord in 1:r){
      X<-cbind(X,x^ord)
      X.ts<-cbind(X.ts,x.ts^ord)
    }
    p<-r+1
    Pr<-NULL
    for (rr in 1:R){
      Y<-f(x,ord=O)+rnorm(N,sd=sd.w)
      DN<-data.frame(cbind(Y,X))

      mod<-lm(Y~.,DN)
      sd.w.hat<-sqrt(sum(mod$residuals^2)/(N-p))
      Remp[r]<-sqrt(mean(mod$residuals^2))

      e.hat.i<-numeric(N)
      Y.ts<-f(x.ts,ord=O)
      data.ts<-data.frame(cbind(Y.ts,X.ts))
      names(data.ts)<-names(DN)
      if (rr==1){
        plot(x.ts,Y.ts,type="l",ylim=c(min(Y),max(Y)))
        points(x,Y)
      }
      pr<-predict(mod,data.ts)
      Pr<-cbind(Pr,pr)
      lines(x.ts,pr,col="red")

    } ## for rr
    lines(x.ts,Y.ts,type="l",ylim=c(min(Y),max(Y)),lwd=5)
    lines(x.ts,apply(Pr,1,mean),type="l",ylim=c(min(Y),max(Y)),lwd=5,col="green")
    bias=mean((Y.ts-apply(Pr,1,mean))^2)
    vvar=mean(apply(Pr,1,var))
    B=c(B,bias)
    V=c(V,vvar)
    title(paste("Bias^2=",round(bias,4),
                ", Var=",round(vvar,2), ", degree=",r))
    cat(r,"\n")
    par(ask=TRUE)
  }

  plot(1:n,B,type="l",main="Bias/variance tradeoff",col="red",xlab="order", ylab="")
  lines(1:n,V,col="green")
  lines(1:n,B+V)
  legend(8,1,c("BIAS^2","VARIANCE","MSE"),text.col=c("red","green","black"))
  list(Remp=Remp,B=B, V=V)
}
