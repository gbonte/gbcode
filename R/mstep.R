


nlcor<-function(x,y){
  require(lazy)
  N<-length(x)
  I<-sample(1:N,round(N/3))
  
  data<-data.frame(x[I],y[I])
  
  y.lazy <- lazy(y ~ x,data,control=lazy.control(linIdPar=c(round(N/2),N)))
  yh<-predict(y.lazy, newdata=x[-I])$h
  
  cor(y[-I], yh)
}







#' KNN.multioutput
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#'
#' @references \emph{Bontempi G. Ben Taieb S. Conditionally dependent strategies for multiple-step-ahead prediction in local learning, International Journal of Forecasting Volume 27, Issue 3, July–September 2011, Pages 689–699}
#' @description Multioutput KNN
#' @details Multioutput KNN which perform a locally constant model with weighted combination of local model on the basis of leave-one-out error. A tricube kernel is used to weight the distances.
#' @title KNN.multioutput
#' @name KNN.multioutput
#' @param X: training input [N,n]
#' @param Y: training output [N,m]
#' @param X.ts: test input [N.ts,n]
#' @param  k: min number of neighbours
#' @param  dist: type of distance: \code{euclidean, cosine}
#' @param  F: forgetting factor
#' @param  C: integer parameter which sets the maximum number of neighbours (Ck)
#' @param  wta: if TRUE a winner-takes-all strategy is used;  otherwise a weigthed combination is done on the basis of the l-o-o error
#' @return vector of N.ts predictions
#' @export
#' @examples
#' ## Multi-step ahead time series forecasting
#' t=seq(0,200,by=0.1)
#' N<-length(t)
#' H<-50 ## horizon prediction
#' TS<-sin(t)
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' E=MakeEmbedded(TS.tr,n=3,delay=0,hor=H,1)
#' X<-E$inp
#' Y<-E$out
#' N<-NROW(X)
#' Y.cont<-KNN.multioutput(X,Y,rev(TS.tr[(N.tr-H):N.tr]))
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont)

KNN.multioutput<- function(X,Y,X.ts,k=10,Di=NULL,dist="euclidean",C=2,F=0,wta=TRUE){
  
  if (k<=0)
    stop("k must be positive")
  
  
  if (is.vector(X))
    X<-array(X,c(length(X),1))
  N<-NROW(X)
  n<-NCOL(X)
  
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  
  if ( k >=NROW(X)){
    return (array(mean(Y),c(N.ts,1)))
  }
  
  if (is.vector(Y) ){
    Y<-array(Y,c(length(Y),1))
  }
  
  m<-NCOL(Y)
  if (n==1)
    X<-array(X,c(N,1))
  out.hat<-array(NA,c(N.ts,m))
  
  if (is.null(Di)){
    if (dist=="euclidean")
      Ds<-dist2(X,X.ts)
    if (dist=="cosine")
      Ds<-dist.cos(X,X.ts)
  } else
    Ds<-Di
  if (m>1){
    w.na<-which(apply(Y,1,ana))
  }else{
    w.na<-which(is.na(Y))
  }
  if (length(w.na)>0)
    Ds[w.na,]<-Inf
  
  if (F <1)
    Ds<-array(Ds*exp(-(F*(1:N))),c(N,N.ts)) ## forgetting factor
  
  
  for (i in 1:N.ts){
    index<-sort(Ds[,i],index.return=TRUE)
    err<-numeric(C*k)+Inf
    oo<-NULL
    
    for (kk in k:(min(NROW(X),C*k))){
      d<-Ds[index$ix[1:kk],i]/Ds[index$ix[kk+1],i]
      ## tricube kernel
      wd<-((1-abs(d)^3)^3)*(abs(d)<1)
      wd<-wd/sum(wd)
      if (any(is.na(wd)))
        wd<-numeric(length(wd))+1/length(wd)
      if (m>1){
        L<-apply(Y[index$ix[1:kk],],2,var) ##constloo,wd)
        L<-L[which(is.finite(L))]
        err[kk]<-mean(L)
        oo<-rbind(oo,apply(wd*Y[index$ix[1:kk],],2,sum,na.rm=T))
        
      } else {
        err[kk]<-constloo(Y[index$ix[1:kk],1],wd)
        oo<-rbind(oo,mean(Y[index$ix[1:kk],1],na.rm=T))
      }
      if (is.na(err[kk])){
        stop("KNN.multioutput error")
      }
    }
    
    
    
    if (wta) {
      out.hat[i,]<-oo[which.min(err)-k+1,]
    }else {
      w<-(1/err[k:(C*k)])/sum(1/err[k:(C*k)])
      ## weights inversely proportional to loo errors
      out.hat[i,]<-apply(oo*w,2,sum,na.rm=T)
    }
    
    
  }
  
  
  out.hat
}


#' KNN.acf
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#'
#' @references \emph{Bontempi G. Ben Taieb S. Conditionally dependent strategies for multiple-step-ahead prediction in local learning, International Journal of Forecasting Volume 27, Issue 3, July–September 2011, Pages 689–699}
#' @description Multioutput KNN
#' @details Multioutput KNN for multi-step-ahed prediction. It performs a locally constant model with weighted combination of local model on the basis of the dynamic properties of the training time series.
#' @title KNN.acf
#' @name KNN.acf
#' @param X: training input [N,n]
#' @param Y: training output [N,m]
#' @param X.ts: test input [N.ts,n]
#' @param  k: min number of neighbours
#' @param  dist: type of distance: \code{euclidean, cosine}
#' @param  F: forgetting factor
#' @param  C: integer parameter which sets the maximum number of neighbours (Ck)
#' @param  wta: if TRUE a winner-takes-all strategy is used;  otherwise a weigthed combination is done on the basis of the l-o-o error
#' @param Acf: autocorrelation function of the training series
#' @return vector of N.ts predictions
#' @export
#' @examples
#' ## Multi-step ahead time series forecasting
#' library(lazy)
#' t=seq(0,200,by=0.1)
#' N<-length(t)
#' H<-500 ## horizon prediction
#' TS<-sin(t)+rnorm(N,sd=0.1)
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' E=MakeEmbedded(TS.tr,n=3,delay=0,hor=H,1)
#' X<-E$inp
#' Y<-E$out
#' N<-NROW(X)
#' Y.cont<-KNN.acf(X,Y,rev(TS.tr[(N.tr-H):N.tr]),TS=TS.tr)
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont)
KNN.acf<- function(X,Y,X.ts,k=10,dist="euclidean",C=2,F=0,Acf,Pacf,TS,del=0){
  if (k<=0)
    stop("k must be positive")
  
  if (is.vector(X))
    X<-array(X,c(length(X),1))
  N<-NROW(X)
  n<-NCOL(X)
  
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.tsts<-array(X.ts,c(1,n))
  }  else {
    if (  n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  if ( k >=NROW(X)){
    return (array(mean(Y),c(N.ts,1)))
  }
  
  m<-NCOL(Y)
  if (n==1)
    X<-array(X,c(N,1))
  out.hat<-array(NA,c(N.ts,m))
  
  
  Ds<-dist2(X,X.ts)
  
  Ds[which(apply(Y,1,ana))]<-Inf
  Ds<-array(Ds*exp(-(F*(1:N))),c(N,N.ts))
  
  for (i in 1:N.ts){
    
    index<-sort(Ds[,i],index.return=TRUE)
    err<-numeric(C*k)+Inf
    err2<-numeric(C*k)+Inf
    oo<-NULL
    aa<-NULL
    ord.emb<-round(NCOL(X)/2)
    ETS<-MakeEmbedded(array(TS,c(length(TS),1)),n=ord.emb,0)
    for (kk in k:(C*k)){
      d<-Ds[index$ix[1:kk],i]/Ds[index$ix[kk+1],i]
      wd<-((1-abs(d)^3)^3)*(abs(d)<1)
      wd<-wd/sum(wd)
      if (any(is.na(wd)))
        wd<-1/length(wd)+numeric(length(wd))
      
      if (m>1){
        LP<-apply(Y[index$ix[1:kk],],2,mean,na.rm=T)
        oo<-rbind(oo,LP)
        
        ets<-MakeEmbedded(array(c(LP),c(length(LP),1)),n=ord.emb,0)
        L<-apply(Y[index$ix[1:kk],],2,constloo,wd)
        L<-L[which(is.finite(L))]
        
        err[kk]<-0
        for (jj in 1:ord.emb) {
          
          err[kk]<-err[kk]+mean((ets$out-
                                   lazy.pred(X=ETS$inp[,1:jj],Y=ETS$out,
                                             X.ts=ets$inp[,1:jj],linPar=c(5,10),class=FALSE))^2)
        }
        
        err2[kk]<-mean(L)
        
      } else {
        stop("Only for multi step ahead prediction")
      }
      if (is.na(err[kk]))
        browser()
    }
    
    w2<-(1/err2[k:(C*k)])/sum(1/err2[k:(C*k)])
    
    out.hat[i,]<-oo[which.min(err)-k+1,]
    
  }
  
  
  out.hat
}



#' KNN.acf.lin
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#'
#' @references \emph{Bontempi G. Ben Taieb S. Conditionally dependent strategies for multiple-step-ahead prediction in local learning, International Journal of Forecasting Volume 27, Issue 3, July–September 2011, Pages 689–699}
#' @description Multioutput KNN
#' @details Multioutput KNN for multi-step-ahed prediction. It performs a locally constant model with weighted combination of local model on the basis of the autocorrelation and partial correlation properties of the training time series.
#' @title KNN.acf.lin
#' @name KNN.acf.lin
#' @param X: training input [N,n]
#' @param Y: training output [N,m]
#' @param X.ts: test input [N.ts,n]
#' @param  k: min number of neighbours
#' @param  dist: type of distance: \code{euclidean, cosine}
#' @param  F: forgetting factor
#' @param  C: integer parameter which sets the maximum number of neighbours (Ck)
#' @param  wta: if TRUE a winner-takes-all strategy is used;  otherwise a weigthed combination is done on the basis of the l-o-o error
#' @param Acf: autocorrelation function of the training series
#' @return vector of N.ts predictions
#' @export
#' @examples
#' ## Multi-step ahead time series forecasting
#'
#' t=seq(0,200,by=0.1)
#' N<-length(t)
#' H<-500 ## horizon prediction
#' TS<-sin(t)+rnorm(N,sd=0.1)
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' E=MakeEmbedded(TS.tr,n=3,delay=0,hor=H,1)
#' X<-E$inp
#' Y<-E$out
#' N<-NROW(X)
#' ACF.lag<-5
#' Y.cont<-KNN.acf.lin(X,Y,rev(TS.tr[(N.tr-H):N.tr]),Acf=acf(TS.tr,lag.max=ACF.lag,plot=F)$acf,Pacf=pacf(TS.tr,lag.max=ACF.lag,plot=F)$acf,TS=TS.tr)
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont)
KNN.acf.lin<- function(X,Y,X.ts,k=10,dist="euclidean",C=2,F=0,Acf,Pacf,TS,del=0){
  
  if (k<=0)
    stop("k must be positive")
  
  
  if (is.vector(X))
    X<-array(X,c(length(X),1))
  N<-NROW(X)
  n<-NCOL(X)
  
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  
  if ( k >=NROW(X)){
    
    return (array(mean(Y),c(N.ts,1)))
  }
  
  m<-NCOL(Y)
  if (n==1)
    X<-array(X,c(N,1))
  out.hat<-array(NA,c(N.ts,m))
  
  
  Ds<-dist2(X,X.ts)
  
  Ds[which(apply(Y,1,ana))]<-Inf
  Ds<-array(Ds*exp(-(F*(1:N))),c(N,1))
  
  for (i in 1:N.ts){
    index<-sort(Ds[,i],index.return=TRUE)
    err<-numeric(C*k)+Inf
    err2<-numeric(C*k)+Inf
    oo<-NULL
    
    for (kk in k:(C*k)){
      d<-Ds[index$ix[1:kk],i]/Ds[index$ix[kk+1],i]
      ##d<-numeric(length(d))
      ##wd<-exp(-d^2)
      wd<-((1-abs(d)^3)^3)*(abs(d)<1)
      wd<-wd/sum(wd)
      if (any(is.na(wd)))
        wd<-1/length(wd)+numeric(length(wd))
      
      if (m>1){
        
        LP<-apply(Y[index$ix[1:kk],],2,mean,na.rm=T)
        oo<-rbind(oo,LP)
        A<-acf(c(TS,LP),lag.max=length(Acf)-1,plot=FALSE)$acf
        PA<-pacf(c(TS,LP),lag.max=length(Pacf),plot=FALSE)$acf
        
        err[kk]<- 1-abs(cor(c(PA),c(Pacf)))+1-abs(cor(c(A),c(Acf)))
        
      } else {
        O<-mean(Y[index$ix[1:kk],1],na.rm=T)
        A<-acf(c(O),lag.max=length(Acf)-1,plot=FALSE)$acf
        PA<-pacf(c(O),lag.max=length(Pacf),plot=FALSE)$acf
        err[kk]<-1-abs(cor(c(PA),c(Pacf)))##+1-abs(cor(c(A),c(Acf)))
        wwP<-exp(-0.5*(1:length(PA)))
        wwA<-exp(-0.5*(1:length(A)))
        err[kk]<-mean((wwP*(c(PA)-c(Pacf)))^2)+mean(wwA*((c(A)-c(Acf)))^2)
        
        oo<-rbind(oo,0)
      }
      if (is.na(err[kk]))
        stop("Error")
    }
    out.hat[i,]<-oo[which.min(err)-k+1,]
    
  }
  out.hat
}



#' KNN.pls
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#'
#' @references \emph{Bontempi G. Ben Taieb S. Conditionally dependent strategies for multiple-step-ahead prediction in local learning, International Journal of Forecasting Volume 27, Issue 3, July–September 2011, Pages 689–699}
#' @description Multioutput KNN
#' @details Multioutput KNN for multi-step-ahed prediction. It performs a locally constant model with weighted combination of local model on the basis of partial least-squares error
#' @title KNN.pls
#' @name KNN.pls
#' @param X: training input [N,n]
#' @param Y: training output [N,m]
#' @param X.ts: test input [N.ts,n]
#' @param  k: min number of neighbours
#' @param  dist: type of distance: \code{euclidean, cosine}
#' @param  F: forgetting factor
#' @param  C: integer parameter which sets the maximum number of neighbours (Ck)
#' @param  wta: if TRUE a winner-takes-all strategy is used;  otherwise a weigthed combination is done on the basis of the l-o-o error
#' @param Acf: autocorrelation function of the training series
#' @return vector of N.ts predictions
#' @export
#' @examples
#' ## Multi-step ahead time series forecasting
#' library(pls)
#' t=seq(0,400,by=0.1)
#' N<-length(t)
#' H<-1500 ## horizon prediction
#' TS<-sin(t)+rnorm(N,sd=0.1)
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' E=MakeEmbedded(TS.tr,n=3,delay=0,hor=H,1)
#' X<-E$inp
#' Y<-E$out
#' N<-NROW(X)
#' ACF.lag<-5
#' Y.cont<-KNN.pls(X,Y,rev(TS.tr[(N.tr-H):N.tr]))
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont)
#'
#'
#' ## Multi-step ahead time series forecasting chaotic time series
#' rm(list=ls()
#' N<-NROW(A)
#' H<-200 ## horizon prediction
#' TS<-A[,1]
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' E=MakeEmbedded(TS.tr,n=16,delay=0,hor=H,1)
#' X<-E$inp
#' Y<-E$out
#' N<-NROW(X)
#'
#' Y.cont<-KNN.pls(X,Y,rev(TS.tr[(N.tr-H):N.tr]))
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont)
#'
KNN.pls<- function(X,Y,X.ts,k=10,dist="euclidean",C=2,F=0,del=0){
  
  if (k<=0)
    stop("k must be positive")
  
  
  if (is.vector(X))
    X<-array(X,c(length(X),1))
  N<-NROW(X)
  n<-NCOL(X)
  
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  
  if ( k >=NROW(X)){
    
    return (array(mean(Y),c(N.ts,1)))
  }
  
  m<-NCOL(Y)
  if (n==1)
    X<-array(X,c(N,1))
  out.hat<-array(NA,c(N.ts,m))
  
  init<-1
  Ds<-dist2(X[,init:n],X.ts[,init:n])
  
  Ds[which(apply(Y,1,ana))]<-Inf
  Ds<-array(Ds*exp(-(F*(1:N))),c(N,1))
  
  for (i in 1:N.ts){
    
    index<-sort(Ds[,i],index.return=TRUE)
    err<-numeric(C*k)+Inf
    err2<-numeric(C*k)+Inf
    oo<-NULL
    
    for (kk in k:(C*k)){
      d<-Ds[index$ix[1:kk],i]/Ds[index$ix[kk+1],i]
      XX<-data.frame(X[index$ix[1:kk],])
      names(XX)<-as.character(1:NCOL(XX))
      XXTs<-data.frame(array(X.ts[i,],c(1,n)))
      names(XXTs)<-as.character(1:NCOL(XX))
      YY<-Y[index$ix[1:kk],]
      
      MV<-plsr(YY~.,data=XX,validation="LOO")
      LP<-predict(MV,newdata=XXTs)
      
      nc<-which.min(apply(MV$validation$PRESS,2,mean,na.rm=T))
      
      LP<-c(LP[,,nc])
      
      oo<-rbind(oo,LP)
      err[kk]<-  mean(MV$validation$PRESS[,nc],na.rm=T)
      
      if (is.na(err[kk]))
        stop("Error")
    }
    
    w2<-(1/err[k:(C*k)])/sum(1/err[k:(C*k)])
    
    out.hat[i,]<-apply(oo*w2,2,sum,na.rm=T)
    
  }
  out.hat
}


#' lin.pls
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#'
#' @references \emph{Bontempi G. Ben Taieb S. Conditionally dependent strategies for multiple-step-ahead prediction in local learning, International Journal of Forecasting Volume 27, Issue 3, July–September 2011, Pages 689–699}
#' @description Multioutput lin PLS
#' @details Multioutput PLS for multi-step-ahed prediction. It performs a  partial least-squares 
#' @title lin.pls
#' @name lin.pls
lin.pls<- function(X,Y,X.ts){
  
  if (is.vector(X))
    X<-array(X,c(length(X),1))
  N<-NROW(X)
  n<-NCOL(X)
  
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  
  m<-NCOL(Y)
  if (n==1)
    X<-array(X,c(N,1))
  out.hat<-array(NA,c(N.ts,m))
  
  
  XX<-data.frame(X)
  names(XX)<-as.character(1:NCOL(XX))
  XXTs<-data.frame(X.ts)
  names(XXTs)<-as.character(1:NCOL(XX))
  
  MV<-plsr(Y~.,data=XX,validation="CV")
  LP<-predict(MV,newdata=XXTs)
  nc<-which.min(apply(MV$validation$PRESS,2,mean,na.rm=T))
  
  out.hat<-c(LP[,,nc])
  
  
  
}



#' multiplestepAhead
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#'
#' @references \emph{Bontempi G. Ben Taieb S. Conditionally dependent strategies for multiple-step-ahead prediction in local learning, International Journal of Forecasting Volume 27, Issue 3, July–September 2011, Pages 689–699}
#' @description multiplestepAhead
#' @details Wrapper over a set of methods for multiple step ahead time series prediction
#' @title multiplestepAhead
#' @name multiplestepAhead
#' @param TS: time series
#' @param n: embedding order
#' @param H: horizon
#' @param  Kmin: min number of neighbours
#' @param  dist: type of distance: \code{euclidean, cosine}
#' @param  F: forgetting factor
#' @param  C: integer parameter which sets the maximum number of neighbours (C*k)
#' @param  smooth: if TRUE, the preidction is obtained by averaging multiple windows with different starting points
#' @param  method:
#' \itemize{
#' \item{arima}: prediction based on the \pkg{forecast} package
#' \item{direct}: direct prediction based on \link{KNN.multioutput} function
#' \item{iter}: recursive prediction based on \link{KNN.multioutput} function
#' \item{lazydirect}: direct prediction based on \link{lazy.pred} function
#' \item{lazyiter}: recursive prediction based on \link{lazy.pred} function
#' \item{mimo}: MIMO prediction based on \link{KNN.multioutput} function
#' \item{mimo.comb}: MIMO prediction based on \link{KNN.multioutput} function which combines a set of predictors based on different horizons and different starting points
#' \item{mimo.acf}: MIMO prediction based on \link{KNN.acf} function which combines a set of predictors based on different horizons and different starting points
#' \item{mimo.acf.lin}: MIMO prediction based on \link{KNN.acf.lin} function which combines a set of predictors based on different horizons and different starting points
#' \item{mimo.pls}: MIMO prediction based on \link{KNN.pls} function which combines a set of predictors based on different horizons and different starting points
#' \item{mimo.lin.pls}: MIMO prediction based on Partial Least Squares which combines a set of predictors based on different horizons and different starting points
#' }
#' @return H step ahead predictions
#' @export
#' @examples
#' ## Multi-step ahead time series forecasting
#'
#' t=seq(0,400,by=0.1)
#' N<-length(t)
#' H<-500 ## horizon prediction
#' TS<-sin(t)+rnorm(N,sd=0.1)
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' Y.cont=multiplestepAhead(TS.tr,n=3, H=H,method="mimo")
#'
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont)
#'
#'
#'
#'
#' ## Multi-step ahead time series forecasting Santa Fe  chaotic time series A
#' rm(list=ls()
#' data(A)
#' TS=A
#' N<-1000
#' H<-200
#' TS.tr=TS[1:N,1]
#' TS.ts<-TS[(N+1):(N+H),1]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' Y.mimo=multiplestepAhead(TS.tr,n=12, H=H,method="mimo")
#' Y.mimo.comb=multiplestepAhead(TS.tr,n=12, H=H,method="mimo.comb")
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.mimo,col="red")
#' lines(t[(N-H+1):N],Y.mimo.comb,col="green")
#'
#'
multiplestepAhead<-function(TS,n,H,D=0, method="direct",Kmin=3,C=2,FF=0,smooth=FALSE){
  N<-length(TS)
  TS<-array(TS,c(length(TS),1))
  
  M<-MakeEmbedded(TS,n,D,H,w=1)  ## putting time series in input/output form
  X<-M$inp
  Y<-M$out
  NX=NROW(X)
  select.var=1:NCOL(X)
  if (length(select.var)>10){
    rfs=numeric(NCOL(X))
    for (j in 1:NCOL(Y)){
      fs=mrmr(X,Y[,j])
      rfs[fs]=rfs[fs]+1
    }
    select.var=sort(rfs,decr=TRUE,index=TRUE)$ix[1:5]
    
  }
  q<-TS[seq(N-D,N-n+1-D,by=-1),1]
  ## TS=[TS(1), TS(2),....., TS(N)]
  ##  D=0:  q=[TS(N), TS(N-1),...,TS(N-n+1)]
  switch(method,
         arima={
           fit <- arima(TS,c(n,D,1))
           p<-forecast(fit,h=H)$mean
         },
         direct={
           p<-numeric(H)
           for (h  in 1:H){
             I<-1:(NROW(X))
             p[h]<-KNN.multioutput(X[,select.var],array(Y[,h],c(NX,1)),q[select.var],k=Kmin,C=C,F=FF)
           }
         },
         lazydirect={
           p<-numeric(H)
           
           LPar=c(Kmin,(C+1)*Kmin)*length(select.var)
           LPar[1]=min(LPar[1],NROW(X)-2)
           LPar[2]=min(LPar[2],NROW(X)-1)
           CPar=c(Kmin,C*Kmin)
           CPar[1]=min(CPar[1],NROW(X)-1)
           if (NROW(X) <= 5*NCOL(X))
             LPar=NULL
           CPar=c(Kmin,C*Kmin)
           CPar[1]=min(CPar[1],NROW(X)-1)
           
           for (h  in 1:H){
              if (NROW(X)>7)
               p[h]<-lazy.pred(X[,select.var],array(Y[,h],c(NX,1)),q[select.var],
                             conPar=CPar,linPar=LPar)
              else
                p[h]=mean(Y[,h],na.rm=TRUE)
           }
         },
         lindirect={
           p<-numeric(H)
           for (h  in 1:H){
             p[h]<-lin.pred(X[,select.var],array(Y[,h],c(NX,1)),q[select.var],class=FALSE)
           }
         },
         mimo={
           p<-KNN.multioutput(X[,select.var],Y,q[select.var],k=Kmin,C=C,F=FF)
         },
         mimo.comb={
           pdirect2<-NULL
           
           if (smooth & H >=2) ## start before the end of the series with an horizon H
             for (h  in round(H/2):(H)){ 
               p2<-numeric(H)+NA
               q2<-TS[seq(N-H+h-D,N+1-n-H+h-D,by=-1),1]
               ## TS=[TS(1), TS(2),....., TS(N)]
               ##  D=0:  q2=[TS(N-H+h), TS(N-1-H+h),...,TS(N-n+1-H+h)]
               ##        pred=  [TS(N-H+h+1),...TS(N+h+1)]
               KK<-KNN.multioutput(X[,select.var],Y,q2[select.var],k=Kmin,C=C,F=FF)
               p2[1:h]<-KK[(H-h+1):H]
               pdirect2<-rbind(pdirect2,p2)
             }
           
           for (h  in round(H/2):(H)){ ## start at the end of the series with different horizons h=[H/2,...H]
             p2<-numeric(H)+NA
             q2<-TS[seq(N-D,N+1-n-D,by=-1),1]
             KK<-KNN.multioutput(X[,select.var],Y[,1:h],q2[select.var],k=Kmin,C=C,F=FF)
             p2[1:h]<-KK
             pdirect2<-rbind(pdirect2,p2)
           }
           
           ## combination of different predictions
           p<-apply(pdirect2,2,mean,na.rm=T)
         },
         mimo.acf={
           pdirect3<-NULL
           TS.acf<-TS[,1]
           
           if (smooth & H >=2)
             for (h  in round(H/2):(H)){ ## start before the end of the series with an horizon H
               p2<-numeric(H)+NA
               q2<-TS[seq(N-H+h-D,N+1-n-H+h-D,by=-1),1]
               KK<-KNN.acf(X[,select.var],Y,q2[select.var],k=Kmin,C=C,F=FF,
                           TS=TS.acf,D)
               p2[1:h]<-KK[(H-h+1):H]
               pdirect3<-rbind(pdirect3,p2)
             }
           
           
           for (h  in round(H/2):(H)){
             p2<-numeric(H)+NA
             q2<-TS[seq(N-D,N+1-n-D,by=-1),1]
             KK<-KNN.acf(X[,select.var],Y[,1:h],q2[select.var],k=Kmin,C=C,F=FF,
                         TS=TS.acf,D)
             p2[1:h]<-KK
             pdirect3<-rbind(pdirect3,p2)
           }
           ## combination of different predictions
           p<-apply(pdirect3,2,mean,na.rm=T)
           
         },
         mimo.acf.lin={
           ACF.lag<-5
           pdirect4<-NULL
           TS.acf<-TS ##i-1=N
           
           if (smooth & H >=2)
             for (h  in round(H/2):(H)){ ## start before the end of the series with an horizon H
               p2<-numeric(H)+NA
               q2<-TS[seq(N-H+h-D,N+1-n-H+h-D,by=-1),1]
               KK<-KNN.acf.lin(X[,select.var],Y,q2[select.var],k=Kmin,C=C,F=FF,
                               Acf=acf(TS.acf,lag.max=ACF.lag,plot=F)$acf,
                               Pacf=pacf(TS.acf,lag.max=ACF.lag,plot=F)$acf,TS=TS.acf,D)
               p2[1:h]<-KK[(H-h+1):H]
               pdirect4<-rbind(pdirect4,p2)
             }
           
           
           for (h  in round(H/2):(H)){
             p2<-numeric(H)+NA
             q2<-TS[seq(N-D,N+1-n-D,by=-1),1]
             KK<-KNN.acf.lin(X[,select.var],Y[,1:h],q2[select.var],k=Kmin,C=C,F=FF,
                             Acf=acf(TS.acf,lag.max=ACF.lag,plot=F)$acf,
                             Pacf=pacf(TS.acf,lag.max=ACF.lag,plot=F)$acf,TS=TS.acf,D)
             p2[1:h]<-KK
             pdirect4<-rbind(pdirect4,p2)
             
           }
           ## combination of different predictions
           p<-apply(pdirect4,2,mean,na.rm=T)
         },
         mimo.pls={
           pdirect5<-NULL
           
           if (smooth & H >=2)
             for (h  in round(H/2):(H)){ ## start before the end of the series with an horizon H
               p2<-numeric(H)+NA
               q2<-TS[seq(N-H+h-D,N+1-n-H+h-D,by=-1),1]
               KK<-KNN.pls(X[,select.var],Y,q2[select.var],k=Kmin,C=C,F=FF,D)
               p2[1:h]<-KK[(H-h+1):H]
               pdirect5<-rbind(pdirect5,p2)
             }
           for (h  in round(H/2):(H)){
             p2<-numeric(H)+NA
             q2<-TS[seq(N-D,N+1-n-D,by=-1),1]
             KK<-KNN.pls(X[,select.var],Y[,1:h],q2[select.var],k=Kmin,C=C,F=FF,D)
             p2[1:h]<-KK
             pdirect5<-rbind(pdirect5,p2)
           }
           ## combination of different predictions
           p<-apply(pdirect5,2,mean,na.rm=T)
         },
         mimo.lin.pls={
           pdirect6<-NULL
           if (smooth & H >=2)
             for (h  in round(H/2):(H)){ ## start before the end of the series with an horizon H
               p2<-numeric(H)+NA
               q2<-TS[seq(N-H+h-D,N+1-n-H+h-D,by=-1),1]
               KK<-lin.pls(X[,select.var],Y,q2[select.var])
               p2[1:h]<-KK[(H-h+1):H]
               pdirect6<-rbind(pdirect6,p2)
             }
           
           for (h  in max(1,round(H-2)):(H)){
             p2<-numeric(H)+NA
             q2<-TS[seq(N-D,N+1-n-D,by=-1),1]
             KK<-lin.pls(X[,select.var],Y[,1:h],q2[select.var])
             p2[1:h]<-KK
             pdirect6<-rbind(pdirect6,p2)
           }
           ## combination of different predictions
           p<-apply(pdirect6,2,mean,na.rm=T)
         },
         
         iter={
           piter<-numeric(H)
           for (h  in 1:H){
             piter[h]<-KNN.multioutput(X[,select.var],array(Y[,1],c(NROW(X),1)),q[select.var],k=Kmin,C=C,F=FF)
             q<-c(piter[h],q[1:(length(q)-1)])
           }
           p<-piter
         },
         lazyiter={
           piter<-numeric(H)
           LPar=c(Kmin,C*Kmin)*length(select.var)
           LPar[1]=min(LPar[1],NROW(X)-1)
           LPar[2]=min(LPar[2],NROW(X))
           CPar=c(Kmin,C*Kmin)
           CPar[1]=min(CPar[1],NROW(X)-1)
           for (h  in 1:H){
             piter[h]<-lazy.pred(X[,select.var],array(Y[,1],c(NROW(X),1)),q[select.var],
                                 conPar=CPar,linPar=LPar)
             q<-c(piter[h],q[1:(length(q)-1)])
           }
           p<-piter
         },
         liniter={
           piter<-numeric(H)
           for (h  in 1:H){
             piter[h]<-lin.pred(X[,select.var],array(Y[,1],c(NROW(X),1)),q[select.var],class=FALSE)
             q<-c(piter[h],q[1:(length(q)-1)])
           }
           p<-piter
         }
  )
  p
  
}
