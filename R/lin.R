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
#'
#'
regrlin<-function(X,Y,X.ts=NULL,lambda=1e-3){
  
  n<-NCOL(X) # number input variables
  p<-n+1
  N<-NROW(X) # number training data
  XX<-cbind(array(1,c(N,1)),as.matrix(X))
  QR=qr(XX)
  Q=qr.Q(QR)
  R=qr.R(QR)
  H=Q%*%t(Q)
  if (N>= p){
    
    XXX<-t(R)%*%R  ## t(XX)%*%XX
    if (lambda <0){
      min.MSE.loo<-Inf
      
      for (lambdah in c(seq(1e-3,0.1,length.out=5),seq(0.1,5,by=0.2))){
        H1<-ginv(XXX+lambdah*diag(p))
        beta.hat<-H1%*%t(R)%*%t(Q)%*%Y    ##H1%*%t(XX)%*%Y
        Y.hat<-H%*%Y
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
    
    H1<-ginv(XXX+lambda*diag(p))
    beta.hat<-H1%*%t(R)%*%t(Q)%*%Y ##H1%*%t(XX)%*%Y
    
    Y.hat<-H%*%Y
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
  }  else { ## DUAL VERSION: fat X matrix
    
    XXX<-R%*%t(R)  
    if (lambda <0){ ## automatic selection of lambda parameter
      min.MSE.loo<-Inf
      
      for (lambdah in c(seq(1e-3,0.1,length.out=5),seq(0.1,5,by=0.2))){
        H1<-ginv(XXX+lambdah*diag(N))
        beta.hat<-t(R)%*%H1%*%t(Q)%*%Y  
        H=XX%*%t(R)%*%H1%*%t(Q)
        Y.hat<-H%*%Y
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
    
    H1<-ginv(XXX+lambda*diag(N))
    beta.hat<-t(R)%*%H1%*%t(Q)%*%Y
    H=XX%*%t(R)%*%H1%*%t(Q) ## X (X^T X+ lambda I_p)^{-1} X^T= X (R^T R+ lambda I_p)^{-1} R^T Q^T= X R^T (R R^T+ lambda I_N)^{-1} Q^T
    Y.hat<-H%*%Y
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
}



PRESS <- function(mod) {
  res <- resid(mod)
  hat <- lm.influence(mod)$hat
  list(MSE.loo=mean( (res/(1-hat))^2 ),MSE.emp=mean(res^2))
}




