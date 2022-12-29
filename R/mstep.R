sd_trim <- function(x,trim=0.2, const=TRUE){
  # trimmed sd, where x is a matrix (column-wise)
  x <- as.matrix(x)
  if (const){
    if (trim==0.1){const <- 0.7892}
    else if (trim==0.2){const <- 0.6615}
    else {warning("Did you specify the correct consistency constant for trimming?")}
  }
  else{const <- 1}
  m <- apply(x,2,mean,trim)
  res <- x-rep(1,nrow(x))%*%t(m)
  qu <- apply(abs(res),2,quantile,1-trim)
  sdtrim <- apply(matrix(res[t(abs(t(res))<=qu)]^2,ncol=ncol(x),byrow=FALSE),2,sum)
  sdtrim <- sqrt(sdtrim/((nrow(x)*(1-trim)-1)))/const
  return(sdtrim)
}


periodest<-function(x){
  if (length(x)<20)
    return (1)
  x.spec <- spectrum(x,log="no",span=10,plot=FALSE)
  spx <- x.spec$freq
  spy <- 2*x.spec$spec
  return(round(1/spx[which.max(spy)])) ## period estimation
}

nlcor<-function(x,y){
  require(lazy)
  N<-length(x)
  I<-sample(1:N,round(N/3))
  
  data<-data.frame(x[I],y[I])
  
  y.lazy <- lazy(y ~ x,data,control=lazy.control(linIdPar=c(round(N/2),N)))
  yh<-predict(y.lazy, newdata=x[-I])$h
  
  cor(y[-I], yh)
}



#' timefit
#' @author Gianluca Bontempi  \email{Gianluca.Bontempi@@ulb.be}
#'
#' @description time fitting of time series
#' @details time fitting
#' @title timefit
#' @name timefit
#' @param TS.tr: training time series
#' @param C: number of fitted points
#' @param H: horizon
#' @return vector of H predictions
#' @export
timefit<-function(TS.tr,n,C,H){
  Ntr=length(TS.tr)
  Itr=seq(max(1,Ntr-(C+5)),Ntr)
  Its=seq(Ntr+1,Ntr+H)
  D=data.frame(cbind(Itr,TS.tr[Itr]))
  names(D)<-c('t','ts')
  weights=rev(exp(-(1:length(Itr))))
  weights=weights/max(weights)
  if (n==1)
    mod=lm(ts~ 1,data=D,weights=weights)
  
  if (n>1)
    mod=lm(ts~ poly(t,min(length(Itr),n-1)),data=D,weights=weights)
  
  Dts=data.frame(Its)
  names(Dts)<-c('t')
  
  TS.hat=predict(mod,newdata=Dts)
  return(TS.hat)
}


#' KNN.multioutput
#' @author Gianluca Bontempi  \email{Gianluca.Bontempi@@ulb.be}
#'
#' @references \emph{Bontempi G. Ben Taieb S. Conditionally dependent strategies for multiple-step-ahead prediction in local learning, International Journal of Forecasting Volume 27, Issue 3, July–September 2011, Pages 689–699}
#' @references \url{https://tinyurl.com/sfmlh}
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
#' @param  Reg: number (>1) of null terms to regularise the mean 
#' @return vector of N.ts predictions
#' @export
#' @examples
#' ## Multi-step ahead time series forecasting
#' require(gbcode)
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

KNN.multioutput<- function(X,Y,X.ts,k=10,Di=NULL,
                           dist="euclidean",C=2,F=0,
                           wta=TRUE,scaleX=TRUE,
                           Reg=1){
  
  Reg=max(Reg,1)
  
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
  
  if (scaleX & all(apply(X,2,sd)>0.1)){
    X=scale(X)
    X.ts=scale(X.ts,attr(X,"scaled:center"), attr(X,"scaled:scale"))
  }
  
  if ( k >=NROW(X) ){
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
      wd<-c(((1-abs(d)^3)^3)*(abs(d)<1),numeric(Reg)+0.1)
      wd<-wd/sum(wd)
      if (any(is.na(wd)))
        wd<-numeric(length(wd))+1/length(wd)
      if (m>1){
        #L<-apply(Y[index$ix[1:kk],],2,var) 
        L<-apply(Y[index$ix[1:kk],],2,constloo,wd[1:kk])
        L<-L[which(is.finite(L))]
        err[kk]<-mean(L)
        YY=rbind(Y[index$ix[1:kk],],array(mean(Y),c(Reg,NCOL(Y))))
        ## add as many rows as Reg regularisation null terms
        oo<-rbind(oo,apply(wd*YY,2,sum,na.rm=T))
        
      } else {
        err[kk]<-constloo(Y[index$ix[1:kk],1],wd[1:kk])
        ##err[kk]<-var(Y[index$ix[1:kk],1])
        oo<-rbind(oo,mean(c(Y[index$ix[1:kk],1],numeric(Reg)+mean(Y)),na.rm=T))
      }
      if (is.na(err[kk])){
        stop("KNN.multioutput error")
      }
    }
    
    
    
    if (wta) {
      if ((which.min(err)-k+1)<0)
        stop("(which.min(err)-k+1)<0")
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
#' @author Gianluca Bontempi  \email{Gianluca.Bontempi@@ulb.be}
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
#' @param  Reg: number (>1) of null terms to regularise the mean 
#' @return vector of N.ts predictions
#' @export
#' @examples
#' ## Multi-step ahead time series forecasting
#' require(gbcode)
#' library(lazy)
#' t=seq(0,200,by=0.1)
#' N<-length(t)
#' H<-500 ## horizon prediction
#' TS<-sin(t)+rnorm(N,sd=0.1)
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' n=3
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' E=MakeEmbedded(TS.tr,n=n,delay=0,hor=H,1)
#' X<-E$inp
#' Y<-E$out
#' N<-NROW(X)
#' Y.cont<-KNN.acf(X,Y,rev(TS.tr[(N.tr-n+1):N.tr]),TS=TS.tr)
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont)
KNN.acf<- function(X,Y,X.ts,k=10,dist="euclidean",C=2,F=0,Acf,Pacf,TS,Reg=3){
  Reg=max(Reg,1)
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
      wd<-c(((1-abs(d)^3)^3)*(abs(d)<1),numeric(Reg)+0.1)
      wd<-wd/sum(wd)
      if (any(is.na(wd)))
        wd<-1/length(wd)+numeric(length(wd))
      
      if (m>1){
        YY=rbind(Y[index$ix[1:kk],],array(mean(Y),c(Reg,NCOL(Y))))
        LP<-apply(YY,2,mean,na.rm=T)
        oo<-rbind(oo,LP)
        
        ets<-MakeEmbedded(array(c(LP),c(length(LP),1)),n=ord.emb,0)
        L<-apply(YY,2,constloo,wd)
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
        stop("is.na(err[kk])")
    }
    
    w2<-(1/err2[k:(C*k)])/sum(1/err2[k:(C*k)])
    
    out.hat[i,]<-oo[which.min(err)-k+1,]
    
  }
  
  
  out.hat
}



#' KNN.acf.lin
#' @author Gianluca Bontempi  \email{Gianluca.Bontempi@@ulb.be}
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
#' @param  Reg: number (>1) of null terms to regularise the mean 
#' @return vector of N.ts predictions
#' @export
#' @examples
#' ## Multi-step ahead time series forecasting
#' require(gbcode)
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
#' Y.cont<-KNN.acf.lin(X,Y,rev(TS.tr[(N.tr-H):N.tr]),Acf=acf(TS.tr,lag.max=ACF.lag,plot=FALSE)$acf,Pacf=pacf(TS.tr,lag.max=ACF.lag,plot=FALSE)$acf,TS=TS.tr)
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont)
KNN.acf.lin<- function(X,Y,X.ts,k=10,dist="euclidean",C=2,F=0,Acf,Pacf,TS,Reg=3){
  Reg=max(Reg,1)
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
      wd<-c(((1-abs(d)^3)^3)*(abs(d)<1),numeric(Reg)+0.1)
      wd<-wd/sum(wd)
      if (any(is.na(wd)))
        wd<-1/length(wd)+numeric(length(wd))
      
      if (m>1){
        YY=rbind(Y[index$ix[1:kk],],array(mean(Y),c(Reg,NCOL(Y))))
        LP<-apply(YY,2,mean,na.rm=T)
        oo<-rbind(oo,LP)
        A<-acf(c(TS,LP),lag.max=length(Acf)-1,plot=FALSE)$acf
        PA<-pacf(c(TS,LP),lag.max=length(Pacf),plot=FALSE)$acf
        
        err[kk]<- 1-abs(cor(c(PA),c(Pacf)))+1-abs(cor(c(A),c(Acf)))
        
      } else {
        O<-mean(c(Y[index$ix[1:kk],1],numeric(Reg)+mean(Y)),na.rm=T)
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
#' @author Gianluca Bontempi  \email{Gianluca.Bontempi@@ulb.be}
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
#' require(gbcode)
#' t=seq(0,400,by=0.1)
#' N<-length(t)
#' H<-1500 ## horizon prediction
#' TS<-sin(t)+rnorm(N,sd=0.1)
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' n=3
#' E=MakeEmbedded(TS.tr,n=n,delay=0,hor=H,1)
#' X<-E$inp
#' Y<-E$out
#' ACF.lag<-5
#' Y.cont<-KNN.pls(X,Y,rev(TS.tr[(N.tr-n+1):N.tr]))
#' plot(t[(N-H+1):N],TS.ts)
#' lines(t[(N-H+1):N],Y.cont,col="red")
#'
#'
#' rm(list=ls())
#' require(gbcode)
#' data(A)
#' N<-NROW(A)
#' H<-200 ## horizon prediction
#' TS<-A[,1]
#' TS.tr=TS[1:(N-H)]
#' N.tr<-length(TS.tr)
#' TS.ts<-TS[(N-H+1):N]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' n=16
#' E=MakeEmbedded(TS.tr,n=16,delay=0,hor=H,1)
#' X<-E$inp
#' Y<-E$out
#' Y.cont<-KNN.pls(X,Y,rev(TS.tr[(N.tr-n+1):N.tr]))
#' plot((N-H+1):N,TS.ts)
#' lines((N-H+1):N,Y.cont)
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
#' @author Gianluca Bontempi  \email{Gianluca.Bontempi@@ulb.be}
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
#' @author Gianluca Bontempi  \email{Gianluca.Bontempi@@ulb.be}
#'
#' @references \emph{Bontempi G. Ben Taieb S. Conditionally dependent strategies for multiple-step-ahead prediction in local learning, International Journal of Forecasting Volume 27, Issue 3, July–September 2011, Pages 689–699}
#' @references \url{https://tinyurl.com/sfmlh}
#' @description multiplestepAhead: : univariate multi-step-ahead forecaster
#' @details Wrapper over a set of methods for univariate multi-step-ahead time series forecasting
#' @title multiplestepAhead
#' @name multiplestepAhead
#' @param TS: time series in column format [T,1]
#' @param n: embedding order
#' @param H: horizon
#' @param  dist: type of distance: \code{euclidean, cosine} for lazy methods
#' @param  F: forgetting factor
#' @param  C: integer parameter which sets the maximum number of neighbours (C*k) for lazy methods
#' @param  detrend: real parameter (in [0,1]) which fixes the size of window used for linear detrending the series (0 corresponds to all series and 1 to ten latest terms). If detrend<0  no detrending is carried out
#' @param  smooth: if TRUE, the prediction is obtained by averaging multiple windows with different starting points
#' @param  engin: if TRUE, a number of additional features (related to the quantiles) are engineered and added
#' @param  method:
#' \itemize{
#' \item{arima}: prediction based on the \pkg{forecast} package
#' \item{stat_naive}: naive predictor based on the M4 competition code
#' \item{stat_ses_naive}: prediction based on the M4 competition code
#' \item{stat_naive2}: naive predictor based on the M4 competition code
#' \item{stat_ses}: SES predictor based on the M4 competition code
#' \item{stat_holt}: Holt predictor based on the M4 competition code
#' \item{stat_damped}: prediction based on the M4 competition code
#' \item{stat_theta}: Theta predictor based on the M4 competition code
#' \item{stat_comb}: prediction based on the M4 competition code
#' \item{direct}: direct prediction based on \link{KNN.multioutput} function
#' \item{iter}: recursive prediction based on \link{KNN.multioutput} function
#' \item{lazydirect}: locally linear  direct prediction based on \link{lazy.pred} function
#' \item{clazydirect}: locally constant direct prediction based on \link{lazy.pred} function
#' \item{lazyiter}: recursive prediction based on \link{lazy.pred} function
#' \item{lindirect}: direct prediction based on linear predictor
#' \item{rfdirect}: direct prediction based on Random Forest predictor
#' \item{rfiter}: recursive prediction based on Random Forest predictor
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
#' lines(t[(N-H+1):N],Y.cont,col="red")
#'
#'
#'
#'
#' ## Multi-step ahead time series forecasting Santa Fe  chaotic time series A
#' rm(list=ls())
#' require(gbcode)
#' data(A)
#' TS=A
#' N<-1000
#' H<-200
#' TS.tr=TS[1:N,1]
#' TS.ts<-TS[(N+1):(N+H),1]
#' TS.tr=array(TS.tr,c(length(TS.tr),1))
#' Y.dir=multiplestepAhead(TS.tr,n=12, H=H,method="lazydirect")
#' Y.mimo.comb=multiplestepAhead(TS.tr,n=12, H=H,method="mimo.comb")
#' plot((N-H+1):N,TS.ts,type="l",xlab="",ylab="Santa Fe A series")
#' lines((N-H+1):N,Y.dir,col="red")
#' lines((N-H+1):N,Y.mimo.comb,col="green")
#'
#'
#'
multiplestepAhead<-function(TS,n,H,D=0, method="direct",
                            FF=0,smooth=FALSE,maxfs=6,
                            XC=NULL,detrend=0, forget=-1, engin=FALSE,
                            Kmin=3,C=2,debug=FALSE, learner="rf",
                            verbose=FALSE,...){
  
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  if (NCOL(TS)>1)
    stop("Only for univariate time series")
  if (any(is.na(TS)))
    stop("NA in the series")
  
  N<-length(TS)
  if (H>round(N/2))
    stop("Too long horizon ")
  
  qu=seq(0.1,1,by=0.1)
  
  if ((N-n-H)<10)
    method="stat_naive"
  
  if (forget>0){
    I=min(N-10,max(1,round(N*forget))):N
    TS=TS[I]
    if (!is.null(XC))
      XC=XC[I[1]:NROW(XC),]
    N<-length(TS)
  }
  
  trnd.ts=numeric(H)
  trnd.ts2=numeric(H)
  if (detrend>0){
    S=detectSeason(TS,Ls=N+H,pmin=detrend)
    TS=TS-S$spattern[1:N]-S$strend[1:N]
    trnd.ts<-S$spattern+S$strend
    trnd.ts<-trnd.ts[(N+1):(N+H)]
  }
  
  if (!is.null(XC)){
    trnd2=pred("lin",XC[1:N,],TS,XC[1:N,],classi=FALSE,lambda=1e-2)
    trnd.ts2=pred("lin",XC[1:N,],TS,XC,classi=FALSE,lambda=1e-2)[(N+1):(N+H)]
    TS=TS-trnd2
    trnd.ts=trnd.ts+trnd.ts2
  }
  
  ### Set of statistical methods borrowed from M4 competition 
  
  if (method=="stat_naive"){
    p=StatPredictors1(TS, H , index=1,verbose=F)
    return(c(p+trnd.ts))
  }
  if (method=="stat_ses_naive"){
    p=StatPredictors1(TS, H , index=2,verbose=F)
    return(c(p+trnd.ts))
  }
  if (method=="stat_naive2"){
    p=StatPredictors1(TS, H , index=3,verbose=F)
    return(c(p+trnd.ts))
  }
  
  if (method=="stat_ses"){
    p=StatPredictors1(TS, H , index=4,verbose=F)
    return(c(p+trnd.ts))
  }
  
  if (method=="stat_holt"){
    p=StatPredictors1(c(TS), H , index=5,verbose=F)
    return(c(p+trnd.ts))
  }
  
  if (method=="stat_damped"){
    p=StatPredictors1(c(TS), H , index=6,verbose=F)
    return(c(p+trnd.ts))
  }
  
  if (method=="stat_theta"){
    p=StatPredictors1(c(TS), H , index=7,verbose=F)
    return(c(p+trnd.ts))
  }
  
  if (method=="stat_avg"){
    p=numeric(H)+mean(c(TS))
    return(c(p+trnd.ts))
  }
  
  if (method=="stat_comb"){
    p=StatPredictors1(c(TS), H , index=8,verbose=F)
    return(c(p+trnd.ts))
  }
  if (method=="stat_4theta"){
    p=StatPredictors1(c(TS), H , index=9,verbose=F)
    return(c(p+trnd.ts))
  }
  
  if (method=="mimolin"){
    p=multiridge(array(TS,c(length(TS),1)),n,H,MIMO=TRUE,verbose=verbose,...)$Yhat 
    return(c(p+trnd.ts))
  }
  
  if (method=="mimorr"){
    p=multirr(array(TS,c(length(TS),1)),n,H,B=0,QRdec=FALSE,verbose=verbose,...) 
    return(c(p+trnd.ts))
  }
  if (method=="mimocca"){
    p=multicca(array(TS,c(length(TS),1)),n,H,...) 
    return(c(p+trnd.ts))
  }
  
  ### keras based RNN: it requires keras
  if (method=="rnn"){
    
    p=rnnpred(array(TS,c(length(TS),1)),  H,...)
    return(c(p+trnd.ts))
  }
  
  ### keras based RNN: it requires keras
  if (method=="lstm"){
    
    p=lstmpred(array(TS,c(length(TS),1)),  H,...)
    return(c(p+trnd.ts))
  }
  
  if (method=="gluon"){
    
    p=gluonpred(array(TS,c(length(TS),1)),n=n,  H=H,...)
    return(c(p+trnd.ts))
  }
  
  if (method=="nbeats"){
    
    p=nbeatspred(array(TS,c(length(TS),1)),n=n,  H=H,...)
    return(c(p+trnd.ts))
  }
  
  if (method=="nbeatsens"){
    
    p=nbeatsenspred(array(TS,c(length(TS),1)),n=n,  H=H,...)
    return(c(p+trnd.ts))
  }
  
  if (sd_trim(TS)<0.001 && method !="timefit" )
    return (numeric(H)+TS[1]+trnd.ts)
  TS<-array(TS,c(N,1))
  M <- MakeEmbedded(TS, n, D, H, w = 1)
  
  
  X<-M$inp
  Y<-M$out
  if (n==1)
    X<-array(X,c(length(X),1))
  
  NX=NROW(X)
  
  select.var=1:NCOL(X)
  if (length(select.var)>maxfs ) {
    if (engin){
      X<-cbind(X,t(apply(X,1,quantile,qu)))
    }
    rfs=numeric(NCOL(X))
    for (j in 1:NCOL(Y)){
      fs=mrmr(X,Y[,j],nmax=min(NCOL(X)-1,maxfs))
      
      rfs[fs]=rfs[fs]+1
    }
    
    select.var=sort(rfs,decr=TRUE,index=TRUE)$ix[1:min(maxfs,NCOL(X)-1)]
    
  }
  
  if (length(select.var)>2){
    sdX<-apply(X[,select.var],2,sd)
    ws<-which(sdX<0.01)
    if (length(ws)>0)
      select.var<-setdiff(select.var,ws)
  }
  
  if (length(select.var)<1)
    return(numeric(H)+mean(Y))
  
  if (length(select.var)==1)
    if (sd(X)<0.01)
      return(numeric(H)+mean(Y))
  
  
  q<-TS[seq(N-D,N-n+1-D,by=-1),1]
  if (engin)
    q=c(q,quantile(q,qu))
  
  
  ## TS=[TS(1), TS(2),....., TS(N)]
  ##  D=0:  q=[TS(N), TS(N-1),...,TS(N-n+1)]
  switch(method,
         arima={
           
           fit <- tryCatch(
             {
               arima(TS,arimaorder(auto.arima(TS)))
             },
             error = function(e){
               arima(TS,c(0,D,1))
             }
           )
           
           p<-forecast(fit,h=H)$mean
         },
         timefit={
           p<-timefit(TS,n,C,H)
           
         },
         direct={
           
           p<-numeric(H)
           for (h  in 1:H){
             I<-1:(NROW(X))
             Yh=Y[,h]
             if (length(which(!is.na(Yh)))<1)
               p[h]=0
             else {
               if (length(which(!is.na(Yh)))<NROW(X) ){
                 p[h]<-mean(Yh,na.rm=TRUE)
               }else{
                 
                 p[h]<-KNN.multioutput(X[,select.var],array(Yh,c(NX,1)),
                                       q[select.var],k=Kmin,C=C,F=FF,Reg=1)
               }
             }
           }   
         },
         lazydirect={
           p<-numeric(H)
           for (h  in 1:H){
             wna=which(!is.na(Y[,h]))  
             if (length(wna)<1){
               p[h]=0
               
             }else{
               
               if (length(wna)>9){
                 Xw=array(X[wna,select.var],c(length(wna),length(select.var)))
                 Yw=array(Y[wna,h],c(length(wna),1))
                 LPar=c(Kmin,(C+1)*Kmin)*length(select.var)
                 LPar[1]=min(LPar[1],NROW(Xw)-2)
                 LPar[2]=min(LPar[2],NROW(Xw)-1)
                 CPar=c(Kmin,C*Kmin+1)
                 CPar[1]=min(CPar[1],NROW(Xw)-1)
                 CPar[2]=min(CPar[2],NROW(Xw))
                 
                 if (NROW(Xw) <= (7*NCOL(Xw)))
                   LPar=NULL
                 CPar=c(Kmin,C*Kmin)
                 CPar[1]=min(CPar[1],NROW(X)-1)
                 
                 vX=1
                 if (!is.vector(Xw))
                   vX=apply(Xw,2,sd)
                 if (any(vX<0.01)){ 
                   v0=which(vX<0.01)
                   
                   if (length(v0)>= length(select.var)){
                     p[h]=mean(Y[,h],na.rm=TRUE)
                   } else {
                     q2=q[select.var]
                     
                     p[h]<-lazy.pred(Xw[,-v0],Yw,q2[-v0],
                                     conPar=CPar,linPar=LPar,cmbPar=3)
                   }
                 }else{
                   
                   p[h]<-lazy.pred(Xw,Yw,q[select.var],
                                   conPar=CPar,linPar=LPar,cmbPar=3)
                 }
               }else
                 p[h]=mean(Y[,h],na.rm=TRUE)
             }
           } ## for h
         },
         clazydirect={
           p<-numeric(H)
           for (h  in 1:H){
             wna=which(!is.na(Y[,h]))  
             if (length(wna)<1){
               p[h]=0
               
             }else{
               if (length(wna)>9){
                 Xw=X[wna,select.var]
                 Yw=array(Y[wna,h],c(length(wna),1))
                 
                 CPar=c(Kmin,C*Kmin+1)
                 CPar[1]=min(CPar[1],NROW(Xw)-1)
                 CPar[2]=min(CPar[2],NROW(Xw))
                 
                 
                 LPar=NULL
                 
                 
                 vX=1
                 if (!is.vector(Xw))
                   vX=apply(Xw,2,sd)
                 if (any(vX<0.01)){ 
                   v0=which(vX<0.01)
                   
                   if (length(v0)>= length(select.var)){
                     p[h]=mean(Y[,h],na.rm=TRUE)
                   } else {
                     q2=q[select.var]
                     
                     p[h]<-lazy.pred(Xw[,-v0],Yw,q2[-v0],
                                     conPar=CPar,linPar=LPar,cmbPar=3)
                   }
                 }else{
                   
                   p[h]<-lazy.pred(Xw,Yw,q[select.var],
                                   conPar=CPar,linPar=LPar,cmbPar=3)
                 }
               }else
                 p[h]=mean(Y[,h],na.rm=TRUE)
             }
           } ## for h
         },
         rfdirect={
           p<-numeric(H)
           for (h  in 1:H){
             wna=which(!is.na(Y[,h]))
             if (length(wna)<1){
               p[h]=0
             }else{
               if (length(wna)>9){
                 p[h]<-rf.pred(X[wna,select.var],array(Y[wna,h],c(length(wna),1)),q[select.var],
                               class=FALSE,ntree=C*50)
               }else
                 p[h]=mean(Y[,h],na.rm=TRUE)
             }
           }
         },
         mldirect={
           p<-numeric(H)
           for (h  in 1:H){
             wna=which(!is.na(Y[,h]))
             if (length(wna)<1){
               p[h]=0
             }else{
               if (length(wna)>9){
                 p[h]<-pred(learner,X[wna,select.var],array(Y[wna,h],c(length(wna),1)),q[select.var],
                               class=FALSE,ntree=C*50)
               }else
                 p[h]=mean(Y[,h],na.rm=TRUE)
             }
           }
         },
         lindirect={
           
           p<-numeric(H)
           for (h  in 1:H){
             wna=which(!is.na(Y[,h]))
             if (length(wna)<1){
               p[h]=0
             }else{
               if (length(wna)>9){
                 p[h]<-lin.pred(X[wna,select.var],array(Y[wna,h],c(length(wna),1)),q[select.var],
                                class=FALSE,lambda=1e-3*C)
               }else
                 p[h]=mean(Y[,h],na.rm=TRUE)
             }
           } ## for h
         },
         mimo={
           p<-KNN.multioutput(X[,select.var],Y,q[select.var],k=Kmin,C=C,F=FF,Reg=1)
         },
         mimo.comb={
           pdirect2<-NULL
           
           if (smooth & H >=2) ## start before the end of the series with an horizon H
             for (h  in round(H/2):(H)){ 
               p2<-numeric(H)+NA
               q2<-TS[seq(N-H+h-D,N+1-n-H+h-D,by=-1),1]
               if (engin)
                 q2=c(q2,quantile(q2,qu))
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
             if (engin)
               q2=c(q2,quantile(q2,qu))
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
                           TS=TS.acf,D,Reg=1)
               p2[1:h]<-KK[(H-h+1):H]
               pdirect3<-rbind(pdirect3,p2)
             }
           
           
           for (h  in round(H/2):(H)){
             p2<-numeric(H)+NA
             q2<-TS[seq(N-D,N+1-n-D,by=-1),1]
             
             KK<-KNN.acf(X[,select.var],Y[,1:h],q2[select.var],k=Kmin,C=C,F=FF,
                         TS=TS.acf,D,Reg=1)
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
                               Pacf=pacf(TS.acf,lag.max=ACF.lag,plot=F)$acf,TS=TS.acf,D,
                               Reg=1)
               p2[1:h]<-KK[(H-h+1):H]
               pdirect4<-rbind(pdirect4,p2)
             }
           
           
           for (h  in round(H/2):(H)){
             p2<-numeric(H)+NA
             q2<-TS[seq(N-D,N+1-n-D,by=-1),1]
             
             KK<-KNN.acf.lin(X[,select.var],Y[,1:h],q2[select.var],k=Kmin,C=C,F=FF,
                             Acf=acf(TS.acf,lag.max=ACF.lag,plot=F)$acf,
                             Pacf=pacf(TS.acf,lag.max=ACF.lag,plot=F)$acf,TS=TS.acf,D,Reg=1)
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
         mlmimo={
           pdirect3<-NULL
           TS.acf<-TS[,1]
           
           if (smooth & H >=2)
             for (h  in round(H/2):(H)){ ## start before the end of the series with an horizon H
               p2<-numeric(H)+NA
               q2<-TS[seq(N-H+h-D,N+1-n-H+h-D,by=-1),1]
               
               KK<-pred(learner, X[,select.var],Y,q2[select.var],k=Kmin,C=C,F=FF,
                           TS=TS.acf,D,Reg=1)
               p2[1:h]<-KK[(H-h+1):H]
               pdirect3<-rbind(pdirect3,p2)
             }
           
           
           for (h  in round(H/2):(H)){
             p2<-numeric(H)+NA
             q2<-TS[seq(N-D,N+1-n-D,by=-1),1]
             
             KK<-pred(learner,X[,select.var],Y[,1:h],q2[select.var],k=Kmin,C=C,F=FF,
                         TS=TS.acf,D,Reg=1)
             p2[1:h]<-KK
             pdirect3<-rbind(pdirect3,p2)
           }
           ## combination of different predictions
           p<-apply(pdirect3,2,mean,na.rm=T)
           
         },
         iter={
           piter<-numeric(H)
           for (h  in 1:H){
             piter[h]<-KNN.multioutput(X[,select.var],array(Y[,1],c(NROW(X),1)),
                                       q[select.var],k=Kmin,C=C,F=FF)
             q<-c(piter[h],q[1:(length(q)-1)])
             
           }
           p<-piter
         },
         lazyiter={
           piter<-numeric(H)
           LPar=c(Kmin,C*Kmin)*length(select.var)
           LPar[1]=min(LPar[1],NROW(X)-1)
           LPar[2]=min(LPar[2],NROW(X))
           if (NROW(X) <= (7*length(select.var)))
             LPar=NULL
           
           CPar=c(Kmin,C*Kmin)
           CPar[1]=min(CPar[1],NROW(X)-1)
           for (h  in 1:H){
             piter[h]<-lazy.pred(X[,select.var],array(Y[,1],c(NROW(X),1)),q[select.var],
                                 conPar=CPar,linPar=LPar,cmbPar=3)
             q<-c(piter[h],q[1:(length(q)-1)])
             
           }
           p<-piter
         },
         clazyiter={
           piter<-numeric(H)
           
           LPar=NULL
           
           CPar=c(Kmin,C*Kmin)
           CPar[1]=min(CPar[1],NROW(X)-1)
           for (h  in 1:H){
             piter[h]<-lazy.pred(X[,select.var],array(Y[,1],c(NROW(X),1)),q[select.var],
                                 conPar=CPar,linPar=LPar,cmbPar=3)
             q<-c(piter[h],q[1:(length(q)-1)])
             
           }
           p<-piter
         },
         rfiter={
           piter<-numeric(H)
           for (h  in 1:H){
             piter[h]<-rf.pred(X[,select.var],array(Y[,1],c(NROW(X),1)),q[select.var],
                               class=FALSE,ntree=C*50)
             q<-c(piter[h],q[1:(length(q)-1)])
             
           }
           p<-piter
         },
         liniter={
           piter<-numeric(H)
           for (h  in 1:H){
             
             piter[h]<-lin.pred(X[,select.var],array(Y[,1],c(NROW(X),1)),
                                q[select.var],class=FALSE,lambda=(1e-7)*(10*C))
             q<-c(piter[h],q[1:(length(q)-1)])
             
           }
           p<-piter
         }, 
         stop("multistepAhead: Unknown method")
         
  )
  if (any(is.na(c(p))))
    stop("error in multipleStepAhead")
  c(p+trnd.ts)   ## trend correction
  
}



#' MmultiplestepAhead
#' @author Gianluca Bontempi  \email{Gianluca.Bontempi@@ulb.be}
#'
#' @references \url{https://tinyurl.com/sfmlh}
#' @description MmultiplestepAhead: multi-variate multi-step-ahead forecaster
#' @details Wrapper over a set of methods for multi variate multiple step ahead time series prediction
#' @title MmultiplestepAhead
#' @name MmultiplestepAhead
#' @param TS: time series [T,m] where m>1 is the number of series
#' @param n: embedding order
#' @param dfmlmethods: alternative methods from \link{MmultiplestepAhead} used by DFML to predict factors
#' @param H: horizon
#' @param unimethod: method from \link{MmultiplestepAhead} used to predict each univariate series
#' @param  multi:
#' \itemize{
#' \item{uni}: prediction based on univariate forecasting with unimethod in \link{MmultiplestepAhead}
#' \item{dfm}: prediction based on DFM
#' \item{dfml}: prediction based on DFML
#' \item{VAR}: prediction based on VAR 
#' \item{VARs}: prediction based on VAR shrink from \pkg{VARshrink} package
#' \item{rnn}: prediction based on \pkg{keras} rnn
#' \item{lstm}: prediction based on \pkg{keras} lstm
#' \item{multifs}: prediction based on MISO direct strategy and feature selection (predictor from \link{pred})
#' }
#' @return matrix [H,m] with the H step ahead predictions for m series
#' @export
#' @examples
#' ## Multi-variate Multi-step-ahead time series forecasting
#' rm(list=ls())
#' require(gbcode)
#' require(VARshrink)
#' N=100
#' n=10 
#' Xtr=array(rnorm(N*n),c(N,n))
#' ## random data: only to show the syntax
#' n=3
#' H=10
#' Xhat1=MmultiplestepAhead(Xtr,n,H,multi="uni",uni=c("lazyiter"))
#' Xhat2=MmultiplestepAhead(Xtr,n,H,multi="dfm")
#' Xhat3=MmultiplestepAhead(Xtr,n,H,multi="VARs")
#' Xhat4=MmultiplestepAhead(Xtr,n,H,multi="dfml",cdfml=3,
#'                         dfmlmethods=c("lindirect","lazydirect"))
#' Xhat5=MmultiplestepAhead(Xtr,n,H,multi="multifs",mod="rf")
#' 
MmultiplestepAhead<-function(TS,n=1,H=1,D=0, multi="uni",
                             unimethod="stat_naive", 
                             dfmlmodels="lindirect",
                             mod="rf",
                             pc0=2,
                             verbose=FALSE,
                             debug=FALSE,...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  m<-NCOL(TS)
  N=NROW(TS)
  
  if (m<=1)
    stop("Only for multivariate series")
  Yhat=array(NA,c(H,m))
  if (multi=="uni")
    for (j in 1:m){
      Yhat[,j]=multiplestepAhead(TS[,j],n,H,D=D, method=unimethod,...)
    }
  if (multi=="rnn")
    Yhat=pyrnnpredgpt(TS,H,...)
  if (multi=="lstm")
    Yhat=pylstmpredgpt(TS,H,n,...)
  if (multi=="pylstm") 
    Yhat=pylstmpredgpt(TS,H,n,...)
  if (multi=="dfm"){
    Yhat=dfml(TS,n,H,p0=pc0,dfmod=dfmlmodels[1],...)
  }
  if (multi=="kfm"){
    Yhat=kfml(TS,n,H,p0=pc0,adaptive=FALSE,...)
  }
  if (multi=="kfm2"){
    Yhat=kfml(TS,n,H,p0=pc0,adaptive=TRUE,...)
  }
  if (multi=="dfml"){
    ## DFML searches in the space: #Pcomponents(1:cdfml*pc0)
    # #models(dfmlmodels), autoregressive order (1:cdfml*n)
    Ddesign=dfmldesign(TS,cdfml*n,H,p0=cdfml*pc0,models=dfmlmodels,...)
    
    Yhat=dfml(TS,Ddesign$m,H,p0=Ddesign$p,dfmod=Ddesign$mod,...)
    if (verbose){
      print(Ddesign)
    }
    
  }
  if (multi=="VAR"){
    if (n*(m^2)>N)
      n=1
    
    Yhat=VARpred2(TS,m=MTS::VAR(TS,p=n,output=FALSE),h=H,Out=FALSE)$pred 
  }
  if (multi=="VARs"){
    
    Yhat=VARspred(TS,n,H)
  }
  if (multi=="multifs")
    Yhat=multifs(TS,n,H,mod=mod,debug=debug,...)
  
  if (multi=="multiridge")
    Yhat=multiridge(TS,n,H,mod=mod,MIMO=TRUE,direct=FALSE,...)$Yhat
  if (multi=="whitenridge")
    Yhat=whitenridge(TS,n,H,mod=mod,MIMO=FALSE,direct=TRUE,...)$Yhat
  if (multi=="directridge")
    Yhat=multiridge(TS,n,H,mod=mod,MIMO=FALSE,direct=TRUE,...)$Yhat
  if (multi=="multidiridge")
    Yhat=multiridge(TS,n,H,mod=mod,MIMO=TRUE,direct=TRUE,...)$Yhat
  if (multi=="multiteridge1")
    Yhat=multiteridge(TS,n,H,Hobj=1,mod=mod,...)$Yhat
  if (multi=="multiteridgeH")
    Yhat=multiteridge(TS,n,H,Hobj=H,mod=mod,...)$Yhat
  if (multi=="ensridge")
    Yhat=ensridge(TS,n,H,mod=mod,...)$Yhat
  if (multi=="multiridge2")
    Yhat=multiridge(TS,n,H,mod=mod,maha=TRUE,MIMO=TRUE,...)$Yhat
  
  if (multi=="unimultiridge"){
    MR=multiridge(TS,n,H,mod=mod,MIMO=TRUE,...)
    Yhat=MR$Yhat
    for (i in 1:m){
      MRi=multiridge(TS[,i],n,H,mod=mod,MIMO=TRUE,...)
     
      if (MRi$MSE<MR$MSE[i])
        Yhat[,i]=MRi$Yhat
    }
    return(Yhat)
  }
  if (multi=="multirr")
    Yhat=multirr(TS,n,H,mod=mod,...)
  if (multi=="multilasso")
    Yhat=multiml(TS,n,H,mod=mod,learner="py.lasso_regr",...)
  if (multi=="multirf")
    Yhat=multiml(TS,n,H,mod=mod,learner="py.rf_regr",...)
  if (multi=="multikeras")
    Yhat=multiml(TS,n,H,mod=mod,learner="py.keras_regr",...)
  if (multi=="multicca")
    Yhat=multicca(TS,n,H,mod=mod,...)
  if (multi=="multipls")
    Yhat=multipls(TS,n,H,mod=mod,...)
  if (multi=="comb"){
    YYhat=array(NA,c(H,m,3))
    YYhat[,,1]=multifs(TS,n,H,mod=mod,...)
    YYhat[,,2]=VARspred(TS,n,H,mod=mod,...)
    YYhat[,,3]=dfml(TS,n,H,p0=pc0,...)
    
    Yhat=apply(YYhat,c(1,2),mean)
  }
  if (any(is.na(Yhat)))
    stop(paste(multi," is an unknown method in MmultiplestepAhead"))
  
  return(Yhat)
}
