#### regrlin ####
#' Linear regression
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Linear regression with PRESS leave one out estimate
#'@name regrlin
#'@param X: training input
#'@param Y: training output
#'@param X.ts: test input
#'@param lambda: regularization parameter
#'@return a list with fields:
#'\itemize{
#' \item{\code{e}}: training error,
#' \item{ \code{beta.hat}:} coeffcients,
#' \item{ \code{MSE.emp}:} training MSE,
#' \item{ \code{sdse.emp}:} standard deviation of squared error,
#' \item{ \code{var.hat}:} estimated noise variance;
#' \item{ \code{MSE.loo}:} PRESS MSE,
#' \item{ \code{sdse.loo}:} standard deviation of squared leave-one-out error error,
#' \item{ \code{Y.hat}:} predicted training output,
#' \item{ \code{Y.hat.ts}:} predicted test output,
#' \item{ \code{e.loo}:} leave-one-out error
#'}
#'
#'@examples
#'N<-100
#' n<-10
#' X<-array(rnorm(N*n),c(N,n))
#' Y<-sin(X[,1]*X[,2])
#' R<-regrlin(X,Y,X)
#' R2<-regrlin.d(X,Y,X,lambda=1e-5)
#'
#'
regrlin<-function(X,Y,X.ts=NULL,lambda=1e-3){

  n<-NCOL(X) # number input variables
  p<-n+1
  N<-NROW(X) # number training data

  XX<-cbind(array(1,c(N,1)),as.matrix(X))

  if (lambda <0){
    min.MSE.loo<-Inf
    XXX<-t(XX)%*%XX
    for (lambdah in seq (1e-3,5,by=0.5)){
       H1<-ginv(XXX+lambdah*diag(p))
       beta.hat<-H1%*%t(XX)%*%Y
       H<-XX%*%H1%*%t(XX)
       Y.hat<-XX%*%beta.hat
       e<-Y-Y.hat
       e.loo<-e/(1-diag(H))
       w.na<-which(is.na(e.loo))
       if (length(w.na)>0)
         e.loo[w.na]=1
       MSE.loo<-mean( e.loo^2 )
       if (MSE.loo<min.MSE.loo){
         lambda<-lambdah
         min.MSE.loo<-MSE.loo
       }

     }

  }

  H1<-ginv(t(XX)%*%XX+lambda*diag(p))
  beta.hat<-H1%*%t(XX)%*%Y
  H<-XX%*%H1%*%t(XX)
  Y.hat<-XX%*%beta.hat
  e<-Y-Y.hat
  var.hat.w<-(t(e)%*%e)/(N-p)
  MSE.emp<-mean(e^2)
  e.loo<-e/(1-diag(H))
  MSE.loo<-mean( e.loo^2 )
  NMSE<-mean( e.loo^2 )/(sd(Y)^2)
  Y.hat.ts<-NULL
  if (!is.null(X.ts)){
    N.ts<-NROW(X.ts)
    if (is.vector(X.ts) & n>1 ){
      Y.hat.ts<-c(1,X.ts)%*%beta.hat
      } else {
      XX<-cbind(array(1,c(N.ts,1)),X.ts)
      Y.hat.ts<-XX%*%beta.hat
    }
   }
  list(e=e,beta.hat=beta.hat,
       MSE.emp=MSE.emp,sdse.emp=sd(e^2),var.hat=var.hat.w,MSE.loo=MSE.loo,sdse.loo=sd(e.loo^2),
       Y.hat=Y.hat,Y.hat.ts=Y.hat.ts,e.loo=e.loo)
}



PRESS <- function(mod) {
  res <- resid(mod)
  hat <- lm.influence(mod)$hat
  list(MSE.loo=mean( (res/(1-hat))^2 ),MSE.emp=mean(res^2))
}



regrlin.d<-function(X,Y,X.ts=NULL,lambda=1e-5){

  n<-NCOL(X) # number input variables
  p<-n+1
  N<-NROW(X) # number training data
  N.ts<-NROW(X.ts)
  XX<-cbind(array(1,c(N,1)),X)
  G<-XX%*%t(XX)  ## [N,N]
  H1<-ginv(t(XX)%*%XX+lambda*diag(p))

  H<-XX%*%H1%*%t(XX)%*%Y-ginv(G)%*%Y


  alpha.hat<-ginv(G+lambda*diag(N))%*%Y ##[N,1]
  beta.hat<-t(XX)%*%alpha.hat
  Y.hat<-G%*%alpha.hat

  e<-Y-Y.hat
  var.hat.w<-(t(e)%*%e)/(N-p)

  K<- cbind(array(1,c(N.ts,1)),X.ts)%*%t(XX) ##[Nts,N]

  MSE.emp<-mean(e^2)



  Y.hat.ts<-K%*%alpha.hat


  list(e=e,beta.hat=beta.hat,alpha.hat=alpha.hat,
       MSE.emp=MSE.emp,sdse.emp=sd(e^2),var.hat=var.hat.w,
       Y.hat=Y.hat,Y.hat.ts=Y.hat.ts)
}


regrlazy<-function(X,Y,X.ts=NULL,conPar=1,linPar=2,cmbPar=10,return.more=FALSE){
  n<-NCOL(X)
  d<-data.frame(cbind(Y,X))
  names(d)[1]<-"Y"
  names(d)[2:(n+1)]<-paste("x",1:n,sep="")

  mod<-lazy(Y~.,d,control=lazy.control(distance="euclidean",
                    conIdPar=conPar,
                    linIdPar=linPar,
                    cmbPar=cmbPar))
  if (is.vector(X.ts) & n>1)
    X.ts<-array(X.ts,c(1,n))
  d.ts<-data.frame(X.ts)

  names(d.ts)<-names(d)[2:(n+1)]

  if (!return.more){
    ll<- predict(mod,d.ts)
    return(ll$h)

  } else {
    ll<- predict(mod,d.ts,S.out=T,k.out=F)
  }
  return(ll)
}


