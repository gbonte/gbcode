#' AUC
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description AUC
#' @details AUC
#' @title AUC
#' @name AUC
#' @export
#'
#' @param  y: real value
#' @param  yhat: predicted probability
#' @return AUC
#' @export
#' @examples
#' ## random prediction
#' library(ROCR)
#' AUC(round(runif(100)),rnorm(100))
#'
AUC<-function(y,yhat){

  p<-prediction(yhat,y)
  p<-performance(p,"auc")

mean(unlist(slot(p,"y.values")),na.rm=T)
}




MI<-function(y,yhat){

  p<-prediction(yhat,y)
  p<-performance(p,"mi")

mean(unlist(slot(p,"y.values")),na.rm=T)
}
MXE<-function(y,yhat){
  yhat[which(yhat>=1-1e-3)]<-1-1e-3
  yhat[which(yhat<=1e-3)]<-1e-3
  p<-prediction(yhat,y)
  p<-performance(p,"mxe")

median(unlist(slot(p,"y.values")),na.rm=T)
}
RMSE<-function(y,yhat){

  p<-prediction(yhat,y)
  p<-performance(p,"rmse")

mean(unlist(slot(p,"y.values")),na.rm=T)
}
SAR<-function(y,yhat){

  p<-prediction(yhat,y)
  p<-performance(p,"sar")

mean(unlist(slot(p,"y.values")),na.rm=T)
}
BER<-function(y,yhat,thr=0.5){
  N<-length(y)
  L<-levels(y)
  if (!is.factor(yhat)){
    w0<-which(yhat<=thr)
    w1<-which(yhat>thr)
  } else {
    w1<-which(yhat==L[2])
    w0<-which(yhat==L[1])

  }



  tp<-length(which(y[w1]==L[2]))
  fp<-length(which(y[w1]==L[1]))
  tn<-length(which(y[w0]==L[1]))
  fn<-length(which(y[w0]==L[2]))

  (fp+fn)/N


  B1<-fp/(fp+tn)
  B2<-fn/(fn+tp)
  if ((fp+tn)==0)
    B1<-0
  if ((fn+tp)==0)
    B2<-0

  return(0.5*(B1+B2))
}
MCC<-function(y,yhat,thr=0.5){
  N<-length(y)
  L<-levels(y)
  if (!is.factor(yhat)){
    w0<-which(yhat<=thr)
    w1<-which(yhat>thr)
  } else {
    w1<-which(yhat==L[2])
    w0<-which(yhat==L[1])

  }



  tp<-length(which(y[w1]==L[2]))
  fp<-length(which(y[w1]==L[1]))
  tn<-length(which(y[w0]==L[1]))
  fn<-length(which(y[w0]==L[2]))

  S<-(tp+fn)/N
  P<-(fp+tp)/N



  return((tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
}


ERR<-function(y,yhat){
  ##
  w0<-which(y=="0")
  w1<-which(y=="1")

  if (length(w0)==0)
    return(mean((as.numeric(y[w1])-yhat[w1])^2))
  if (length(w1)==0)
    return(mean((as.numeric(y[w0])-yhat[w0])^2))
  mean(mean((as.numeric(y[w0])-yhat[w0])^2),
       mean((as.numeric(y[w1])-yhat[w1])^2))
}
Fscore2<-function(y,yhat,thr=0.5,beta=1){
  N<-length(y)
  F<-NULL
  P<-NULL
  R<-NULL
  for (thr in  seq(min(yhat),max(yhat),length.out=100)){


    w0<-which(yhat<=thr)
    w1<-which(yhat>thr)
    tp<-length(which(y[w1]=="1"))
    fp<-length(which(y[w1]=="0"))
    tn<-length(which(y[w0]=="0"))
    fn<-length(which(y[w0]=="1"))

    precision<-tp/(tp+fp)
    recall<-tp/(tp+fn)
    P<-c(P,precision)
    R<-c(R,recall)
    F<-c(F,(1+beta^2)*(precision*recall)/(beta^2*precision+recall))
  }

  return(mean(F,na.rm=T))
}
Fscore<-function(y,yhat){

  p<-prediction(yhat,y)
  p<-performance(p,"f")

mean(unlist(slot(p,"y.values")),na.rm=T)
}


PRcurve2<-function(y,yhat,yhat2,thr=0.5,beta=1){
  N<-length(y)
  F<-NULL
  P<-NULL
  R<-NULL
  for (thr in seq(min(yhat),max(yhat),length.out=100)){


    w0<-which(yhat<=thr)
    w1<-which(yhat>thr)
    tp<-length(which(y[w1]=="1"))
    fp<-length(which(y[w1]=="0"))
    tn<-length(which(y[w0]=="0"))
    fn<-length(which(y[w0]=="1"))

    precision<-tp/(tp+fp)
    recall<-tp/(tp+fn)
    P<-c(P,precision)
    R<-c(R,recall)
    F<-c(F,(1+beta^2)*(precision*recall)/(beta^2*precision+recall))
  }

  F2<-NULL
  P2<-NULL
  R2<-NULL
  for (thr in  seq(min(yhat),max(yhat),length.out=100)){


    w0<-which(yhat2<=thr)
    w1<-which(yhat2>thr)
    tp<-length(which(y[w1]=="1"))
    fp<-length(which(y[w1]=="0"))
    tn<-length(which(y[w0]=="0"))
    fn<-length(which(y[w0]=="1"))

    precision<-tp/(tp+fp)
    recall<-tp/(tp+fn)
    P2<-c(P2,precision)
    R2<-c(R2,recall)
    F2<-c(F2,(1+beta^2)*(precision*recall)/(beta^2*precision+recall))
  }

  browser()
  plot(R,P,col="red")
  lines(R2,P2,col="green")

}
PRcurve<-function(y,yhat,yhat2,thr=0.5,beta=1){

  p<-prediction(yhat,y)
  perf<-performance(p,"prec","rec")
   p2<-prediction(yhat2,y)
  perf2<-performance(p2,"prec","rec")

  plot(perf,col="red")
  lines(unlist(perf2@x.values),unlist(perf2@y.values),col="green")
}
MCN<-function(y,yhat,yhat2,thr=0.5){
  N<-length(y)

  w0<-which(yhat<=thr)
  w1<-which(yhat>thr)
  w0.2<-which(yhat2<=thr)
  w1.2<-which(yhat2>thr)

  y.hat<-numeric(N)
  y.hat2<-numeric(N)
  y.hat[w1]<-1
  y.hat2[w1.2]<-1

  e01<-length(which(factor(y.hat) !=y & factor(y.hat2)==y))
  e10<-length(which(factor(y.hat) ==y  & factor(y.hat2)!=y))

  chi<-((abs(e01-e10)-1)^2)/(e01+e10)

  return(pchisq(chi,df=1,lower=F))

}


Fsc<-function(pr,rec,beta=1){
  if (pr+rec==0)
    return(0)
  (1+beta^2)*pr*rec/((beta^2)*pr+rec)
}

auprc<-function(est,pos){
  nc<-length(pos)
  pr<-0
  re<-0
  for (ii in 1:length(est)){
    tp<-length(intersect(est[1:ii],pos))
    fp<-ii-tp
    pr<-c(pr,tp/(tp+fp))
    re<-c(re,tp/nc)
  }

  u.re<-unique(re)
  if (length(u.re)==1)
    return(0)
  auprc<-0
  for (u in 2:length(u.re)){
    allr<-which(re==u.re[u])
    auprc<-auprc+max(pr[allr])*(u.re[u]-u.re[u-1])
  }





  auprc
}
