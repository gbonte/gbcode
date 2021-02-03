
#### pred ####
#' Wrapper on learning algoritmhs
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Wrapper on learning algoritmhs for regression and classification
#' @name pred
#' @export
#'@param algo: learning algoritmh: \code{"lin"}: linear, \code{"rf"}: \pkg{randomForest}, \code{"svm"}: \pkg{e1071} , \code{"lazy"}: \pkg{lazy}, \code{"gbm"}: \pkg{gbm},\code{"gam"}: \pkg{gam}, \code{"nb"}: \pkg{e1071}, \code{"lasso"}: \pkg{lars},\code{"mboost"}: \pkg{mboost}
#'@param X: training input
#'@param Y: training output
#'@param X.ts: test input
#'@param classi: TRUE for classification, FALSE for regression
#'@param ...: parameters of the learning algoritmh from the original package
#'@return if \code{classi=FALSE} predicted test output; if \code{classi=TRUE} a list with
#' \itemize{
#' \item{\code{pred}:}  predicted class
#' \item{ \code{prob}:} posteriori probability
#'}
#'  
#'@examples
#'## regression example
#' library(randomForest)
#' n=4
#' N=1000
#' X=array(rnorm(N*n),c(N,n))
#' Y=X[,1]*X[,2]+rnorm(N,sd=0.1)
#' Itr=sample(N,round(N/2))
#' Its=setdiff(1:N,Itr)
#' Xtr=X[Itr,]
#' Ytr=Y[Itr]
#' Xts=X[Its,]
#' Yts=Y[Its]
#' Yhat=pred("rf",Xtr,Ytr,Xts,ntree=1000, classi=FALSE)
#' e=Yts-Yhat
#' ## normalized mean squared error
#' NMSE=mean(e^2)/var(Yts)
#' print(NMSE)
#'
#' ## classification example
#' n=4
#' N=1000
#' X=array(rnorm(N*n),c(N,n))
#' Y=numeric(N)
#' Y[which(X[,1]*X[,2]+rnorm(N,sd=0.1)>0)]<-1
#' Y=factor(Y)
#' Itr=sample(N,round(N/2))
#' Its=setdiff(1:N,Itr)
#' Xtr=X[Itr,]
#' Ytr=Y[Itr]
#' Xts=X[Its,]
#' Yts=Y[Its]
#' Yhat=pred("lda",Xtr,Ytr,Xts,classi=TRUE)$pred
#' e=as.numeric(Yts!=Yhat)
#' ## misclassification error
#' MISCL=sum(e)
#' print(MISCL/N)
#'
pred<-function(algo="svm",X,Y,X.ts,classi=TRUE,...){
  
  if (algo=="rf")
    P<-rf.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="svm")
    P<-svm.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="ksvm")
    P<-ksvm.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="lda")
    P<-lda.pred(X,Y,X.ts,class=classi)
  
  if (algo=="qda")
    P<-qda.pred(X,Y,X.ts,class=classi)
  
  if (algo=="nb"){
    if (!classi)
      stop("Naive Bayes only for classification")
    P<-nb.pred(X,Y,X.ts,class=classi)
  }
  
  if (algo=="tree")
    P<-tree.pred(X,Y,X.ts,class=classi,...)
  if (algo=="glm")
    P<-glm.pred(X,Y,X.ts,class=classi,...)
  if (algo=="gam")
    P<-gam.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="lazy")
    P<-lazy.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="knn")
    P<-KNN.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="gbm")
    P<-gbm.pred(X,Y,X.ts,class=classi,...)
  if (algo=="gp")
    P<-gp.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="mboost")
    P<-mboost.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="xgboost")
    P<-xgboost.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="stacked")
    P<-stacked.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="FNN")
    P<-FNN.pred(X,Y,X.ts,class=classi,...)
  if (algo=="nnet")
    P<-nn.pred(X,Y,X.ts,class=classi,k=algoptions.i)
  
  if (algo=="rocchio")
    P<-rocchio.pred(X,Y,X.ts,class=classi)
  
  
  if (algo=="lasso")
    P<-lasso.pred(X,Y,X.ts,class=classi,...)
  
  
  if (algo=="logistic")
    P<-logistic.pred(X,Y,X.ts,class=classi)
  
  if (algo=="multinom")
    P<-multinom.pred(X,Y,X.ts,class=classi)
  
  if (algo=="lin")
    P<-lin.pred(X,Y,X.ts,class=classi,...)
  
  if (algo=="boost")
    P<-boosting.pred(X,Y,X.ts,class=classi,...)
  if (algo=="arc")
    P<-arcing.pred(X,Y,X.ts,class=classi,...)
  if (is.null(P))
    stop("Error in mlearn: learner not available")
  
  return (P)
  
  
}


rf.pred<- function(X,Y,X.ts,class=FALSE,...){
  
  n<-NCOL(X)
  N<-NROW(X)
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  if (n==1){
    X<-array(X,c(N,1))
    X.ts<-array(X.ts,c(N.ts,1))
  }
  #set.seed(N*n)
  d<-data.frame(Y,X)
  names(d)[1]<-"Y"
  
  mod.rf<-randomForest(Y~.,data=d,...)
  d.ts<-data.frame(X.ts)
  names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
  p<-predict(mod.rf,d.ts,type="response")
  
  if (class){
    P<-predict(mod.rf,d.ts,type="prob")
    return(list(pred=p,prob=P))
  } else {
    
    
    return(p)
  }
  
}
svm.pred<- function(X,Y,X.ts,proba=TRUE,class=TRUE,...){
  
  n<-NCOL(X)
  N<-NROW(X)
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  if (n==1){
    X<-array(X,c(N,1))
    X.ts<-array(X.ts,c(N.ts,1))
  }
  if (class){
    weights<-NULL
    if (is.factor(Y)){
      L<-levels(Y)
      weights<-numeric(length(L))
      for (i in 1:length(L))
        weights[i]<-1-length(which(Y==L[i]))/length(Y)
      names(weights)<-L
      
      
    }
    u<-unique(Y)
    
    if (length(u)==1){
      P<-array(0,c(NROW(X.ts),length(L)))
      colnames(P)<-L
      P[,u]<-1
      out.hat<-factor(rep(as.character(u),length(X.ts)),levels=L)
      return(list(pred=out.hat,prob=P))
    }
    
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    PP<-NULL
    if (!is.null(weights)) {
      mod.svm<-svm(Y~.,data=d,
                   class.weights=weights,probability=proba,tol=0.05,...)
    } else {
      
      mod.svm<-svm(Y~.,data=d,probability=proba,tol=0.05,...)
    }
    d.ts<-data.frame(X.ts)
    
    
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    
    
    
    p<-predict(mod.svm,d.ts,probability=proba)
    PP<-NULL
    if (proba){
      P<-attr(p,"probabilities")
      PP<-array(0,c(NROW(X.ts),length(L)))
      colnames(PP)<-L
      
      PP[,colnames(P)]<-P
    }
    
    return(list(pred=p,prob=PP))
  } else {  ## if class
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    d.ts<-data.frame(X.ts)
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    
    mod.svm<-svm(Y~.,data=d,...)
    
    return(predict(mod.svm,d.ts))
  }
}


ksvm.pred<- function(X,Y,X.ts,proba=TRUE,class=TRUE,degree=1,...){
  
  n<-NCOL(X)
  N<-NROW(X)
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  if (n==1){
    X<-array(X,c(N,1))
    X.ts<-array(X.ts,c(N.ts,1))
  }
  if (class){
    weights<-NULL
    if (is.factor(Y)){
      L<-levels(Y)
      weights<-numeric(length(L))
      for (i in 1:length(L))
        weights[i]<-1-length(which(Y==L[i]))/length(Y)
      names(weights)<-L
      
      
    }
    u<-unique(Y)
    
    if (length(u)==1){
      P<-array(0,c(NROW(X.ts),length(L)))
      colnames(P)<-L
      P[,u]<-1
      out.hat<-factor(rep(as.character(u),length(X.ts)),levels=L)
      return(list(pred=out.hat,prob=P))
    }
    
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    PP<-NULL
    
    
    
    mod.svm<-ksvm(Y~.,data=d,prob.model=proba,kpar=list(degree=degree),tol=0.05,...)
    
    d.ts<-data.frame(X.ts)
    
    
    
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    
    
    
    p<-predict(mod.svm,d.ts,type="probabilities")
    
    return(list(pred=L[apply(p,1,which.max)],prob=p))
  } else {  ## if class
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    d.ts<-data.frame(X.ts)
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    mod.svm<-ksvm(Y~.,data=d,
                  kernel=kernel,...)
    
    return(predict(mod.svm,d.ts))
  }
}

lazy.pred.bin<- function(X,Y,X.ts,conPar=3,linPar=5,cmbPar=10,return.more=F){
  n<-NCOL(X)
  N<-NROW(X)
  
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
  ll<- predict(mod,d.ts)
  prob<-array(NA,c(length(ll$h),2))
  prob[,2]<-pmax(pmin(ll$h,1),0)
  prob[,1]<-1-prob[,2]
  
  
  return(prob)
}

#### lazy.pred ####
#' Wrapper on lazy learning algoritmhs
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Wrapper on lazy learning algoritmhs for regression and classification
#' @name lazy.pred
#' @export
#'@param X: training input
#'@param Y: training output
#'@param X.ts: test input
#'@param conPar: constant modeling parameters
#'@param linPar: linear modeling parameters
#'@param cmbPar: combination parameters
#'@param classi: TRUE for classification, FALSE for regression
#'@param return.more: TRUE for returning additional \pkg{lazy} parameter
#'@return if \code{classi=FALSE} predicted test output; if \code{classi=TRUE} a list with
#' \itemize{
#' \item{\code{pred}:}  predicted class
#' \item{ \code{prob}:} posteriori probability
#'}
lazy.pred<- function(X,Y,X.ts,class=FALSE,return.more=FALSE,
                     conPar=NULL,linPar=NULL,cmbPar=5,scaleX=TRUE){
  if (is.vector(X)){
    n<-1
    N<-length(X)
  }else{
    n<-NCOL(X)
    N<-NROW(X)
  }
  
  if (is.vector(X.ts) & n>1)
    X.ts<-array(X.ts,c(1,n))
  if (is.vector(X.ts) & n==1)
    X.ts<-array(X.ts,c(length(X.ts),1))
  
  
  if (scaleX){
    X=scale(X)
    X.ts=scale(X.ts,attr(X,"scaled:center"), attr(X,"scaled:scale"))
    if (any(is.na(X))| any(is.na(X.ts)))
      return(mean(Y))
##      stop("NA values")
  }
  
  if (class){ ## classification
    l.Y<-levels(Y)
    L<-length(l.Y)
    u<-unique(Y)
    
    if (length(u)==1){
      P<-array(0,c(NROW(X.ts),L))
      colnames(P)<-l.Y
      P[,u]<-1
      out.hat<-factor(rep(as.character(u),length(X.ts)),levels=l.Y)
      return(list(pred=out.hat,prob=P))
    }
    
    if (L==2) {
      YY<-numeric(N)
      ind.ll<-which(Y==l.Y[2])
      YY[ind.ll]<-1
      YY[setdiff(1:N,ind.ll)]<-0
      
      pp<-lazy.pred.bin(X,YY,X.ts,conPar=conPar,linPar=linPar,cmbPar=cmbPar)
      colnames(pp)<-l.Y
      
      return(list(pred=factor(l.Y[apply(pp,1,which.max)],levels=l.Y), prob=pp))
    } else {
      algo="lazy"
      
      out.hat<-multiclass(X,Y,X.ts,algo=algo,strategy="oo",conPar=conPar,linPar=linPar,cmbPar=cmbPar)
      return(list(pred=out.hat$class, prob=out.hat$posterior))
      
    }
  } else { ## regression
    d<-data.frame(cbind(Y,X))
    names(d)[1]<-"Y"
    names(d)[2:(n+1)]<-paste("x",1:n,sep="")
    
    
    
    mod<-lazy(Y~.,d,control=lazy.control(distance="euclidean",
                                         conIdPar=conPar,
                                         linIdPar=linPar,
                                         cmbPar=cmbPar))
    
    d.ts<-data.frame(X.ts)
    
    names(d.ts)<-names(d)[2:(n+1)]
    
    
    if (!return.more){
      ll<- predict.lazy(mod,d.ts)
      return(ll$h)
      
    } else {
      ll<- predict.lazy(mod,d.ts,S.out=T,k.out=F)
      return(ll)
    }
  }
  
}



nn.pred<- function(X,Y,X.ts,classi=FALSE,k=5){
  type.out="raw"
  
  if (is.factor(Y) | classi)
    type.out="class"
  
  
  n<-NCOL(X)
  if (is.vector(X.ts)){
    N.ts<-1
  }  else {
    N.ts<-nrow(X.ts)
  }
  
  d<-data.frame(Y,X)
  names(d)[1]<-"Y"
  if (!classi)
    mod.nn<-nnet(Y~.,data=d,size=k,trace=FALSE, linout = TRUE,maxit=1000)
  else
    mod.nn<-nnet(Y~.,data=d,size=k,trace=FALSE, maxit=800)
  d.ts<-data.frame(X.ts)
  
  names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
  
  
  p<-predict(mod.nn,d.ts,type=type.out)
  
  
  if (classi){
    l<-levels(Y)
    return(list(pred=factor(p,levels=l),prob=numeric(N.ts)+NA))
  } else {
    return(p)
  }
}


FNN.pred<- function(X,Y,X.ts,Di=NULL,class=FALSE,k=3,algorithm="brute",condense=FALSE){
  
  if (!class)
    stop("Error in FNN.pred")
  N<-NROW(X)
  N.ts<-NROW(X.ts)
  L<-levels(Y)
  ind<-1:N
  if (condense){
    keep<-condense(X,Y,trace=FALSE)
    keep2<-reduce.nn(X[keep,],keep,Y)
    ind<-keep2
  }
  
  
  predK<-FNN::knn(X[ind,], X.ts, Y[ind], k = k, prob = TRUE,algorithm=algorithm)
  
  
  
  probK<-attr(predK,"prob")
  P<-array(0,c(N.ts,length(L)))
  colnames(P)<-L
  for (i in 1:N.ts){
    P[i,]<-(1-probK[i])/length(L)
    P[i,predK[i]]<-probK[i]
    
  }
  
  return(list(pred=predK,prob=P))
}

KNN.pred<- function(X,Y,X.ts,Di=NULL,class=FALSE,dist="euclidean",k=3){
  
  if (k<=0)
    stop("k must be positive")
  
  if (is.vector(X))
    X<-array(X,c(length(X),1))
  N<-NROW(X)
  n<-NCOL(X)
  
  if (k>N-1)
    k<-(N-1)
  
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  if (is.factor(Y))
    class=TRUE
  if ( k >=NROW(X)){
    if (class)
      return (rep(most.freq(Y),N.ts))
    return (array(mean(Y),c(N.ts,1)))
  }
  
  
  if (n==1)
    X<-array(X,c(N,1))
  out.hat<-array(NA,c(N.ts,1))
  
  
  if (is.null(Di)){
    if (dist=="euclidean")
      Ds<-dist2(X,X.ts)
    if (dist=="cosine")
      Ds<-dist.cos(X,X.ts)
  } else
    Ds<-Di
  if (class){
    l<-levels(Y)
    proba.hat<-array(NA,c(N.ts,length(l)))
    colnames(proba.hat)<-l
  }
  
  
  for (i in 1:N.ts){
    index<-sort(Ds[,i],index.return=TRUE)
    
    if (class){
      cnt<-numeric(length(l))
      names(cnt)<-l
      for (j in 1:k){
        cnt[Y[index$ix[j]]]<-cnt[Y[index$ix[j]]]+1
        
      }
      
      k2<-k
      
      while (index$x[k2+1]==index$x[k2]){
        cnt[Y[index$ix[k2+1]]]<-cnt[Y[index$ix[k2+1]]]+1
        k2<-k2+1
        
        if (k2>=(N-1))
          break
      }
      
      out.hat[i]<-l[which.max(cnt)]
      for (jj in l)
        proba.hat[i,jj]<-cnt[jj]/k
      
      
    } else {
      out.hat[i]<-mean(Y[index$ix[1:k]])
    }
    
  }
  if (class)
    return(list(pred=factor(out.hat,levels=l),prob=proba.hat))
  out.hat
}





lin.pred<- function(X,Y,X.ts,lambda=1e-7,class) {
  
  
  n<-NCOL(X)
  N<-NROW(X)
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  if (n==1){
    X<-array(X,c(N,1))
    X.ts<-array(X.ts,c(N.ts,1))
  }
  if (class){
    L<-levels(Y)
    if (length(L)==2){
      
      ind1<-which(Y==L[1])
      ind2<-which(Y==L[2])
      Y<-numeric(N)
      Y[ind1]<- 0
      Y[ind2]<- 1
    } else {
      algo="lin"
      algoptions=0
      return(multiclass(X,Y,X.ts,algo=algo,algoptions=algoptions,strategy="oo"))
    }
  }
  
  
  d<-data.frame(Y,X)
  names(d)[1]<-"Y"
  
  mod<-lm(Y~.,data=d, tol = lambda)
  
  d.ts<-data.frame(X.ts)
  colnames(d.ts)[1:(n)]<-colnames(d)[2:(n+1)]
  
  out.hat<-predict(mod,d.ts)
  
  if (class){
    pred<-as.character(numeric(NROW(X.ts)))
    pred[which(out.hat>=0.5)]<-L[2]
    pred[which(out.hat<0.5)]<-L[1]
    
    return(list(pred=factor(pred,levels=L),prob=pmax(pmin(1,out.hat),0)))
  } else {
    return(out.hat)
  }
}

multinom.pred<- function(X,Y,X.ts,class=classi) {
  if (class==F)
    stop("Only for classification")
  L<-levels(Y)
  
  
  if (is.vector(X))
    return(lda.pred(X,Y,X.ts))
  
  
  n<-NCOL(X)
  d<-data.frame(Y,X)
  names(d)[1]<-"Y"
  mod<-multinom(Y~.,data=d,trace=F)
  d.ts<-data.frame(X.ts)
  names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
  
  out.hat<-factor(predict(mod,d.ts),levels=L)
  
  list(pred=out.hat)
  
  
}


logistic.pred<- function(X,Y,X.ts,class=classi) {
  if (class==F)
    stop("Only for classification")
  L<-levels(Y)
  
  if (length(L)==2){
    if (is.vector(X))
      return(lda.pred(X,Y,X.ts))
    if (all(Y==L[1]))
      return (rep(L[1],NROW(X.ts)))
    if (all(Y==L[2]))
      return (rep(L[2],NROW(X.ts)))
    ## Y<-as.numeric(Y)-1
    n<-NCOL(X)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    mod<-glm(Y~.,data=d,family=binomial)
    d.ts<-data.frame(X.ts)
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    P<-predict(mod,d.ts,type="response")
    P<-cbind(1-P,P)
    colnames(P)<-L
    
    logistic.hat<-as.numeric(P>0.5)
    out.hat<-L[apply(P,1,which.max)]
    
  } else {
    algo="logistic"
    algoptions=0
    
    out.hat<-multiclass(X,Y,X.ts,algo=algo,algoptions=algoptions,strategy="ex")
  }
  
  list(pred=out.hat,prob=P)
}




## Naive-bayes
nb.pred<- function(X,Y,X.ts,class=TRUE) {
  
  if (class){
    l<-levels(Y)
    n<-NCOL(X)
    if (n==1){
      if (sd(X)==0)
        return(rep(Y[1],length(X.ts)))
      
      X<-data.frame(X,X)
      X.ts<-data.frame(X.ts,X.ts)
      
      
    }
    if (is.vector(X.ts) & n>1){
      N.ts<-1
      X.ts<-array(X.ts,c(1,n))
    }
    u<-unique(Y)
    
    if (length(u)==1){
      P<-array(0,c(NROW(X.ts),length(l)))
      colnames(P)<-l
      P[,u]<-1
      out.hat<-factor(rep(as.character(u),length(X.ts)),levels=l)
      return(list(pred=out.hat,prob=P))
    }
    
    N<-NROW(X)
    n<-NCOL(X)
    d<-data.frame(Y,X)
    colnames(d)[2:(n+1)]<-paste("x",1:n,sep="")
    colnames(d)[1]<-"Y"
    form<-paste("Y",paste("x",as.character(1:n),collapse="+",sep=""),sep="~")
    form<-as.formula(form)
    mod<-naiveBayes(form,d)
    d.ts<-data.frame(X.ts)
    colnames(d.ts)[1:(n)]<-colnames(d)[2:(n+1)]
    
    out.hat<-predict(mod,d.ts,threshold=0.01)
    P<-NULL
    P<-predict(mod,d.ts,type="raw",threshold=0.01)
    w.na<-which(is.nan(apply(P,1,sum)) | is.na(apply(P,1,sum)))
    colnames(P)<-levels(Y)
    if (length(w.na)>0)
      P[w.na,]<-numeric(NCOL(P))+1/NCOL(P)
  } else {
    stop("Naive Bayes only for classification")
  }
  
  
  list(pred=out.hat,prob=P)
  
}



tree.pred<- function(X,Y,X.ts,class=TRUE,...) {
  
  n<-NCOL(X)
  d<-data.frame(Y,X)
  names(d)[1]<-"Y"
  names(d)[2:(n+1)]<-paste("x",1:n,sep="")
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  
  
  mod<-tree("Y~.",data=d,control=tree.control(nobs=NROW(X),...))
  
  d.ts<-data.frame(X.ts)
  names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
  
  if (class){
    
    out.hat<-predict(mod,d.ts,type="class")
    prob<-predict(mod,d.ts,type="vector")
    return(list(pred=out.hat,prob=prob))
  } else {
    out.hat<-predict(mod,d.ts)
    return(out.hat)
  }
  
  
  
}

glm.pred<- function(X,Y,X.ts,class=TRUE,...) {
  
  n<-NCOL(X)
  d<-data.frame(Y,X)
  names(d)[1]<-"Y"
  names(d)[2:(n+1)]<-paste("x",1:n,sep="")
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  
  
  mod<-glm("Y~.",data=d,...)
  
  d.ts<-data.frame(X.ts)
  names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
  
  
  out.hat<-predict(mod,d.ts)
  return(out.hat)
}

gam.pred<- function(X,Y,X.ts,class=TRUE,...) {
  
  n<-NCOL(X)
  d<-data.frame(Y,X)
  names(d)[1]<-"Y"
  names(d)[2:(n+1)]<-paste("x",1:n,sep="")
  if (is.vector(X.ts) & n>1){
    N.ts<-1
    X.ts<-array(X.ts,c(1,n))
  }  else {
    if (n==1)
      N.ts<-length(X.ts)
    else
      N.ts<-nrow(X.ts)
  }
  
  
  
  mod<-gam(Y~.,data=d,family=gaussian)
  
  
  
  d.ts<-data.frame(X.ts)
  names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
  
  
  out.hat<-predict(mod,d.ts)
  return(out.hat)
}






## Linear discriminant
lda.pred.bin<- function(X,Y,X.ts,class=TRUE,...) {
  if (class){
    
    N<-NROW(X)
    n<-NCOL(X)
    
    
    if (is.vector(X.ts) & n>1){
      N.ts<-1
    }  else {
      if (n==1)
        N.ts<-length(X.ts)
      else
        N.ts<-nrow(X.ts)
    }
    
    if (n==1){
      if (sd(X)<1e-10)
        return (array(most.freq(Y),c(N.ts,1)))
    } else {
      ind.Sd<-which(apply(X,2,sd)>1e-10)
      X<-X[,ind.Sd]
      
      if (N.ts==1)
        X.ts<-array(X.ts[ind.Sd],c(N.ts,length(ind.Sd)))
      else
        X.ts<-array(X.ts[,ind.Sd],c(N.ts,length(ind.Sd)))
      if (length(ind.Sd)<1)
        return (array(most.freq(Y),c(N.ts,1)))
    }
    
    
    if ((length(unique(Y))==1))
      return (array(Y[1],c(N.ts,1)))
    n<-NCOL(X)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    
    mod<-lda(Y~.,d)
    d.ts<-data.frame(X.ts)
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    out.hat<-predict(mod,d.ts)
    
    
  } else {
    stop("LDA only for classification")
  }
  out.hat
  
  
}


lda.pred<-function(X,Y,X.ts,class=TRUE,...) {
  if (class==F){
    stop("Only for classification")
  } else {
    L<-levels(Y)
    n<-NCOL(X)
    
    
    if (length(L)==2){
      
      out.hat<-lda.pred.bin(X,Y,X.ts,...)
    } else {
      algo="lda"
      algoptions=0
      
      
      out.hat<-multiclass(X,Y,X.ts,algo=algo,strategy="oo",...)
      
    }
    
  }
  
  
  list(pred=out.hat$class, prob=out.hat$posterior)
  
}

## Quadratic discriminant
qda.pred.bin<- function(X,Y,X.ts,class=TRUE) {
  if (class){
    N.ts<-NROW(X.ts)
    N<-NROW(X)
    n<-NCOL(X)
    if (n==1){
      if (sd(X)<1e-10)
        return (array(most.freq(Y),c(N.ts,1)))
    } else {
      ind.Sd<-which(apply(X,2,sd)>1e-10)
      X<-X[,ind.Sd]
      X.ts<-X.ts[,ind.Sd]
      if (length(ind.Sd)<1)
        return (array(most.freq(Y),c(N.ts,1)))
    }
    
    
    if ((length(unique(Y))==1))
      return (array(Y[1],c(N.ts,1)))
    n<-NCOL(X)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    mod<-qda(Y~.,d)
    d.ts<-data.frame(X.ts)
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    out.hat<-predict(mod,d.ts)
  } else {
    stop("QDA only for classification")
  }
  out.hat
  
  
}


qda.pred<-function(X,Y,X.ts,class=TRUE) {
  if (class==F){
    stop("Only for classification")
  } else {
    L<-levels(Y)
    if (length(L)==2){
      out.hat<-qda.pred.bin(X,Y,X.ts)
    } else {
      algo="qda"
      algoptions=0
      out.hat<-multiclass(X,Y,X.ts,algo=algo,algoptions=algoptions,strategy="ex")
    }
    
  }
  list(pred=out.hat$class, prob=out.hat$posterior)
  
}



lasso.pred<-function(X,Y,X.ts,n.cv=5, class=TRUE,type='lasso') {
  if (class==F){
    
    if (is.vector(X))
      return(lin.pred(X,Y,X.ts,class=F))
    mod<-lars(X,Y,type="lasso")
    cv.lars.fit<-cv.lars(X, Y, K=10,index=seq(from=0, to=1, length=80),plot.it=FALSE)
    # choose fraction based min cv error rule
    min.indx <- which.min(cv.lars.fit$cv)
    s.cvmin <- cv.lars.fit$index[min.indx] 
    out.hat<-predict(mod, newx=X.ts, s=s.cvmin,type="fit", mode="fraction")$fit
    return(out.hat)
  } else {
    L<-levels(Y)
    
    if (length(L)==2){
      
      
      if (is.vector(X))
        return(lda.pred(X,Y,X.ts))
      
      Y<-as.numeric(Y)-1
      
      mod<-lars(X,Y,type="lasso")
      cv.lars.fit<-cv.lars(X, Y, K=10,index=seq(from=0, to=1, length=80),plot.it=FALSE)
      # choose fraction based min cv error rule
      min.indx <- which.min(cv.lars.fit$cv)
      s.cvmin <- cv.lars.fit$index[min.indx] 
      out.hat<-predict(mod, newx=X.ts, s=s.cvmin,type="fit", mode="fraction")$fit
      out.hat<-factor(L[lasso.hat+1],levels=L)
    } else {
      algo="lasso"
      algoptions=n.cv
      
      out.hat<-multiclass(X,Y,X.ts,algo=algo,algoptions=algoptions,strategy="ex")
    }
    
  }
  list(pred=out.hat)
  
}


rocchio.pred<-function(X,Y,X.ts,n.cv=5, class=TRUE,beta=0) {
  L<-levels(Y)
  if (class==F | length(L)>2)
    stop("error")
  N.ts<-NROW(X.ts)
  N<-NROW(X)
  n<-NCOL(X)
  
  if (length(L)==2){
    
    if (n==1){
      return (lda.pred(X,Y,X.ts))
    }
    
    
    ind1<-which(Y==L[1])
    ind2<-which(Y==L[2])
    profile1<-apply(X[ind1,],2,mean)-beta*apply(X[ind2,],2,mean)
    profile2<-apply(X[ind2,],2,mean)-beta*apply(X[ind1,],2,mean)
    
    D<-dist.cos(X.ts,rbind(profile1,profile2))
    
    out.hat<-L[apply(D,1,which.min)]
    
  } else {
    algo="rocchio"
    algoptions=0
    out.hat<-multiclass(X,Y,X.ts,algo=algo,algoptions=algoptions,strategy="ex")
  }
  list(pred=out.hat)
}



which.equal<-function(x,e){
  which(x==e)
}

choose.rand<-function(x,L){
  if (length(x)==0)
    return (sample(L,size=1))
  if (length(x)==1)
    return(x)
  sample(x,size=1)
}

nearest.code<-function(cc,code){
  D<-as.matrix(dist(rbind(cc,code)))
  I<-sample(2:(NROW(code)+1))
  I[which.min(D[1,I])]-1
}

dist.code<-function(cc,code){
  D<-as.matrix(dist(rbind(cc,code)))
  D[1,2:NCOL(D)]
}

multiclass<-function(X.tr,Y.tr,X.ts,algo,strategy="ex",...){
  ### 3 strategies:
  ### aaa: 1 classifier per label
  ### ex: coding of each label
  ### ooo: a classifier for each pair
  
  if (!is.factor(Y.tr))
    stop("Y is not a factor")
  
  L<-levels(Y.tr)
  k<-length(L)
  
  if (k<=2)
    return(list(pred(algo=algo,X.tr,Y.tr,X.ts,classi=T,...)$pred))
  
  
  ## if (strategy=="aaa"){
  ##   YY.ts<-NULL
  
  ##   for (l in 1: length(L)){
  ##     YY.tr<-Y.tr
  ##     ind1<-which(Y.tr==L[l])
  ##     YY.tr[ind1]<-1
  ##     YY.tr[setdiff(1:length(Y.tr),ind1)]<-0
  ##     YY.ts<-cbind(YY.ts,as.character(
  ##                                     pred(algo=algo,X.tr,YY.tr,X.ts,
  ##                                          algoptions=algoptions,classi=T)) )
  ##   }
  
  ##   which.out<-apply(YY.ts,1,which.equal,1)
  ##   if (is.list(which.out)){
  ##     which.out<-unlist(lapply(which.out,choose.rand,length(L)))
  ##   }
  
  ##   return(factor(L[which.out]))
  ## }
  
  if (strategy=="aaa") ## 1 classifier per label
    code<-diag(k)
  
  if (strategy=="ex"){  ## coding of the label
    code<-array(NA,c(k,2^(k-1)))
    code[1,]<-rep(1,2^(k-1))
    for (i in 2:k){
      cnt<-0
      for (j in 1:2^(i-2)){
        code[i,(cnt+1):(cnt+2^(k-i))]<-rep(1,2^(k-i))
        cnt<-(cnt+2^(k-i))
        code[i,(cnt+1):(cnt+2^(k-i))]<-rep(0,2^(k-i))
        cnt<-(cnt+2^(k-i))
      }
    }
    code<-code[,-1]
  }
  YY.ts<-NULL
  if (strategy !="oo"){
    no.classifiers<-NCOL(code)
    
    for (i in 1:no.classifiers){
      YY.tr<-array(NA,length(Y.tr))
      w.1<-which(code[,i]==1)
      ind1<- which(Y.tr%in%L[w.1])
      YY.tr[ind1]<-"1"
      YY.tr[setdiff(1:length(Y.tr),ind1)]<-"0"
      YY.tr<-factor(YY.tr,levels=c("0","1"))
      
      YY.ts<-cbind(YY.ts,as.character(
        pred(algo=algo,X.tr,YY.tr,X.ts,
             classi=T,...)$pred) )
      
    }
    
    which.out<-apply(YY.ts,1,nearest.code,code)
    
    probpost<-NULL
    ## posterior probability inversely proportional to the distance of the class code
    for (yy in 1:NROW(YY.ts)){
      poster<-dist.code(YY.ts[yy,],code)
      poster<-poster/sum(poster)
      poster<-(1-poster)/sum(1-poster)
      probpost<-rbind(probpost,poster)
    }
    
    
    
    
    return(list(class=factor(L[which.out],levels=L),posterior=poster))
  } else { ## all pairs
    for (i in 1:(length(L)-1))
      for (j in (i+1):length(L)){
        ind1<-which(Y.tr==L[i])
        ind2<-which(Y.tr==L[j])
        
        YY.ts<-cbind(YY.ts,
                     as.character(pred(algo=algo,X.tr[c(ind1,ind2),],
                                       factor(Y.tr[c(ind1,ind2)],
                                              levels=L[c(i,j)]),X.ts,
                                       classi=T,...)$pred) )
        
      }
    probpost<-NULL
    ## posterior probability  proportional to the frequence of the class code
    for (yy in 1:NROW(YY.ts)){
      poster<-which.freq(YY.ts[yy,],u=L)
      probpost<-rbind(probpost,poster)
    }
    return(list(class=factor(apply(YY.ts,1,most.freq,u=L),levels=L),posterior=poster))
  }
  
  
  
  
}





#### bagging.pred ####
#' Bagging wrapper on algoritmhs
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Wrapper on lazy learning algoritmhs for regression and classification
#' @name bagging.pred
#' @export
#'@param X: training input
#'@param Y: training output
#'@param X.ts: test input
#'@param classi: TRUE for classification, FALSE for regression
#'@param return.more: TRUE for returning additional \pkg{lazy} parameter
#'@return if \code{classi=FALSE} predicted test output; if \code{classi=TRUE} a list with
#' \itemize{
#' \item{\code{pred}:}  predicted class
#' \item{ \code{prob}:} posteriori probability
#'}
bagging.pred<-function(algo,X,Y,X.ts,B=10,maxN=Inf,classi=TRUE,balanced=FALSE,bagvar=0,...){
  N<-NROW(X)
  n<-NCOL(X)
  
  if (B==1)
    return(pred(algo,X,Y,X.ts,classi=classi,...))
  if (classi){
    L<-levels(Y)
    C<-NULL
    P<-list()
    P[[length(L)+1]]<-0
    
    if (balanced){
      Ll<-NULL
      for (l in 1:length(L))
        Ll<-c(Ll,length(which(Y==L[l])))
    }
    for ( b in 1:B){
      Ib<-NULL
      set.seed(b)
      if (balanced){
        for (l in 1:length(L)){
          Il<-which(Y==L[l])
          Ib<-c(Ib,sample(Il,min(Ll),rep=T))
        }
      }else {
        Ib<-sample(1:N,min(N,maxN),rep=T)
      }
      
      sel<-1:n
      if (bagvar>0)
        sel<-sample(sel,sample(3:min(n,max(3,bagvar)),1))
      
      PR<-pred(algo,X[Ib,sel],Y[Ib],X.ts[,sel],classi=classi,...)
      C<-cbind(C,as.character(PR$pred))
      
      
      for (l in 1:length(L))
        P[[l]]<-cbind(P[[l]],PR$prob[,L[l]])
      
    }
    
    out<-factor(apply(C,1,most.freq),levels=levels(Y))
    Pr<-NULL
    for (l in 1:length(L))
      Pr<-cbind(Pr,apply(P[[l]],1,mean))
    colnames(Pr)<-L
    
    
    out<-L[apply(Pr,1,which.max)]
    
    
    return(list(pred=out,prob=Pr))
  }
}


boosting.pred<-function(X,Y,X.ts,B2=1,algo2="tree",classi=TRUE,...){
  N<-NROW(X)
  n<-NCOL(X)
  
  if (B2==1)
    return(pred(algo2,X,Y,X.ts,classi=classi,...))
  
  
  if (classi){
    L<-levels(Y)
    C<-NULL
    P<-list()
    P[[length(L)+1]]<-0
    
    w<-rep(1/N,N)
    misc<-rep(NA,B2);
    alpha<-rep(NA,B2)
    ws<-rep(1/n,n)
    
    for ( b in 1:B2){
      
      Ib<-sample(seq(1,N),prob=w,replace=TRUE)
      Is<-1:n ## sample(seq(1,n),prob=ws,replace=TRUE)
      p<-pred(algo2,X[Ib,Is],Y[Ib],X[,Is],...)$pred
      
      misc[b] <- sum(w*as.integer(Y != p))/sum(w)
      alpha[b]<-log((1-misc[b])/misc[b])
      
      ew<-numeric(N)+1
      ew[which(p==Y)]<- -1
      w<- w*exp(alpha[b]*ew)
      
      w<-w/sum(w)
      
      if (misc[b]>=0.49)
        w<-rep(1/N,N)
      
      PR<-pred(algo2,X[Ib,],Y[Ib],X.ts,classi=classi,...)
      
      for (l in 1:length(L))
        P[[l]]<-cbind(P[[l]],alpha[b]*PR$prob[,L[l]])
    } ## for b
    
    Pr<-NULL
    for (l in 1:length(L))
      Pr<-cbind(Pr,apply(P[[l]],1,sum)/sum(alpha))
    colnames(Pr)<-L
    
    out<-L[apply(Pr,1,which.max)]
    
    
    return(list(pred=out,prob=Pr))
  }
}


arcing.pred<-function(X,Y,X.ts,B2=1,algo2="tree",classi=TRUE,...){
  N<-NROW(X)
  n<-NCOL(X)
  
  if (B2==1)
    return(pred(algo2,X,Y,X.ts,classi=classi,...))
  
  
  if (classi){
    L<-levels(Y)
    C<-NULL
    P<-list()
    P[[length(L)+1]]<-0
    
    w<-rep(1/N,N)
    misc<-rep(NA,B2);
    mi<-rep(0,N)
    alpha<-numeric(B2)+NA
    
    wf<-rep(1/n,n)
    mif<-array(NA,c(B2,n))
    mif[1,]<-0.1
    for ( b in 1:B2){
      
      Ib<-sample(seq(1,N),prob=w,replace=TRUE)
      ## If<-sample(1:n,sample(2:(n-2),1),prob=wf,replace=FALSE)
      p<-pred(algo2,X[Ib,],Y[Ib],X[,],...)$pred
      
      misc[b] <- sum(w*as.integer(Y != p))/sum(w)
      alpha[b]<-1 ##log((1-misc[b])/misc[b])
      
      mi<-mi+as.integer(Y != p)
      ## mif[b,If]<-(1-misc[b])
      
      ## smif<-apply(mif,2,mean,na.rm=T)
      ## wf<-smif/sum(smif)
      w<- (1+mi^4)/(sum(1+mi^4))
      
      
      PR<-pred(algo2,X[Ib,],Y[Ib],X.ts,classi=classi,...)
      
      for (l in 1:length(L))
        P[[l]]<-cbind(P[[l]],alpha[b]*PR$prob[,L[l]])
    } ## for b
    
    
    
    
    Pr<-NULL
    for (l in 1:length(L))
      Pr<-cbind(Pr,apply(P[[l]],1,sum)/sum(alpha))
    colnames(Pr)<-L
    
    
    out<-L[apply(Pr,1,which.max)]
    
    
    return(list(pred=out,prob=Pr))
  }
  
  
  
  if (!classi){
    
    C<-NULL
    PR<-NULL
    
    w<-rep(1/N,N)
    
    mi<-rep(0,N)
    alpha<-numeric(B2)+NA
    
    wf<-rep(1/n,n)
    mif<-array(NA,c(B2,n))
    mif[1,]<-0.1
    for ( b in 1:B2){
      
      Ib<-sample(seq(1,N),N,prob=w,replace=TRUE)
      ## If<-sample(1:n,sample(2:(n-2),1),prob=wf,replace=FALSE)
      
      p<-pred(algo2,X[Ib,],Y[Ib],X[,],classi=classi,...)
      
      e<-abs(Y-p)
      e<-e/sum(e)
      mi<-mi+e
      ## mif[b,If]<-(1-misc[b])
      
      ## smif<-apply(mif,2,mean,na.rm=T)
      ## wf<-smif/sum(smif)
      w<- (1+mi^4)/(sum(1+mi^4))
      
      PR<-cbind(PR,pred(algo2,X[Ib,],Y[Ib],X.ts,classi=classi,...))
      
      
    } ## for b
    
    
    
    
    Pr<-apply(PR,1,mean)
    
    return(Pr)
  }
}
boost.pred<-function(X,Y,X.ts,classi,...){
  
  l.Y<-levels(Y)
  if (!classi || length(l.Y)!=2)
    stop("Ada boost can be used only for binary classification")
  
  Y<-as.numeric(Y)-1
  X<-as.matrix(X)
  X.ts<-as.matrix(X.ts)
  colnames(X.ts)<-paste("x",1:NCOL(X.ts),sep="")
  colnames(X)<-colnames(X.ts)
  
  
  fit<-adaboost(X,Y,X.ts,...)
  
  
  pred<-factor(round(fit[,NCOL(fit)]),levels=c(0,1),labels=l.Y)
  
  prob<-cbind(1-fit[,NCOL(fit)],fit[,NCOL(fit)])
  colnames(prob)<-l.Y
  
  list(prob=prob,pred=pred)
  
}


gp.pred<-function(X,Y,X.ts,classi,ntrees=1000,...){
  
  
  n<-NCOL(X)
  if (classi){
    
    l.Y<-levels(Y)
    Y<-as.numeric(Y)-1
    X<-as.matrix(X)
    X.ts<-as.matrix(X.ts)
    colnames(X.ts)<-paste("x",1:NCOL(X.ts),sep="")
    colnames(X)<-colnames(X.ts)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    
    gbmodel<-gbm(Y~.,data=d,n.trees=ntrees,verbose=FALSE,...)
    
    d.ts<-data.frame(X.ts)
    
    
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    pred<-predict(gbmodel,d.ts,n.trees=ntrees,type="response")
    
    
    
    prob<-cbind(1-pred,pred)
    colnames(prob)<-l.Y
    pred<-l.Y[apply(prob,1,which.max)]
    
    list(prob=prob,pred=pred)
  } else {
    
    
    X<-as.matrix(X)
    X.ts<-as.matrix(X.ts)
    colnames(X.ts)<-paste("x",1:NCOL(X.ts),sep="")
    colnames(X)<-colnames(X.ts)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    
    gbmodel<-gausspr(Y~.,data=d, kernel="rbf",type="regression",
                     sigma=1)
    
    d.ts<-data.frame(X.ts)
    
    
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    return(predict(gbmodel,d.ts,type="response"))
  }
  
}
gbm.pred<-function(X,Y,X.ts,classi,ntrees=1000,...){
  
  l.Y<-levels(Y)
  n<-NCOL(X)
  if (classi){
    
    
    Y<-as.numeric(Y)-1
    X<-as.matrix(X)
    X.ts<-as.matrix(X.ts)
    colnames(X.ts)<-paste("x",1:NCOL(X.ts),sep="")
    colnames(X)<-colnames(X.ts)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    
    gbmodel<-gbm(Y~.,data=d,n.trees=ntrees,verbose=FALSE,...)
    
    d.ts<-data.frame(X.ts)
    
    
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    pred<-predict(gbmodel,d.ts,n.trees=ntrees,type="response")
    
    
    
    prob<-cbind(1-pred,pred)
    colnames(prob)<-l.Y
    pred<-l.Y[apply(prob,1,which.max)]
    
    list(prob=prob,pred=pred)
  } else {
    
    
    X<-as.matrix(X)
    X.ts<-as.matrix(X.ts)
    colnames(X.ts)<-paste("x",1:NCOL(X.ts),sep="")
    colnames(X)<-colnames(X.ts)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    
    gbmodel<-gbm(Y~.,data=d,n.trees=ntrees,verbose=FALSE,...)
    
    d.ts<-data.frame(X.ts)
    
    
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    return(predict(gbmodel,d.ts,n.trees=ntrees,type="response"))
  }
  
}

mboost.pred<-function(X,Y,X.ts,classi,ntrees=1000,...){
  
  
  n<-NCOL(X)
  ##  if (!classi || length(l.Y)!=2)
  ##    stop("Ada boost can be used only for binary classification")
  
  if (classi){
    l.Y<-levels(Y)
    Y<-as.numeric(Y)-1
    X<-as.matrix(X)
    X.ts<-as.matrix(X.ts)
    colnames(X.ts)<-paste("x",1:NCOL(X.ts),sep="")
    colnames(X)<-colnames(X.ts)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    
    ##gbmodel<-mboost(Y~.,data=d,control=boost_control(...),baselearner="btree")
    gbmodel<-blackboost(Y~.,data=d,control=boost_control(...))
    d.ts<-data.frame(X.ts)
    
    
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    pred<-pmin(pmax(0,predict(gbmodel,d.ts)),1)
    
    
    
    prob<-cbind(1-pred,pred)
    colnames(prob)<-l.Y
    
    pred<-l.Y[apply(prob,1,which.max)]
    
    return(list(prob=prob,pred=pred))
  }else {
    
    X<-as.matrix(X)
    X.ts<-as.matrix(X.ts)
    colnames(X.ts)<-paste("x",1:NCOL(X.ts),sep="")
    colnames(X)<-colnames(X.ts)
    d<-data.frame(Y,X)
    names(d)[1]<-"Y"
    
    
    gbmodel<-gamboost(Y~.,data=d,control=boost_control(...))
    
    d.ts<-data.frame(X.ts)
    
    
    names(d.ts)[1:(n)]<-names(d)[2:(n+1)]
    return(predict(gbmodel,d.ts))
    
    
    
  }
  
}

xgboost.pred<-function(X,Y,X.ts,classi,...){
  
  
  n<-NCOL(X)
  
  if (classi){
    stop("The only thing that XGBoost does is a regression. ")
  }else {
    
    X<-data.matrix(X)
    X.ts<-data.matrix(X.ts)
  
    bst <- xgboost(data =X, label = Y,verbose=0,...)
    Yhat<-predict(bst,X.ts)
    
   return(Yhat)
    
    
    
  }
  
}




stacked.pred<-function(X,Y,X.ts,classi,M=20,R=Inf,algo2="lazy",...){
  
  l.Y<-levels(Y)
  N<-NROW(X)
  n<-NCOL(X)
  Nts<-NROW(X.ts)
  if (!(classi && length(l.Y)==2))
    stop("Stacked can be used only for binary classification")
  
  Y<-as.numeric(Y)-1
  X<-as.matrix(X)
  
  Ir<-sample(N,min(R,N))
  
  Yp<-array(NA,c(length(Ir),M))
  Yts<-array(NA,c(Nts,M))
  for (m in 1:M){
    for (i in 1:length(Ir)){
      if (algo2=="lazy")
        Yp[i,m]<-pred(algo2,X[-Ir[i],],Y[-Ir[i]],X[Ir[i],],conPar=c(3,5+m),classi=FALSE)
      if (algo2=="tree")
        Yp[i,m]<-pred(algo2,X[-Ir[i],],Y[-Ir[i]],X[Ir[i],],
                      mincut=1+m,classi=FALSE)
      if (algo2=="rf")
        Yp[i,m]<-pred(algo2,X[-Ir[i],],Y[-Ir[i]],X[Ir[i],],
                      mtry=m,classi=FALSE)
    }
    if (algo2=="lazy")
      Yts[,m]<-pred(algo2,X,Y,X.ts,conPar=c(3,5+m),classi=FALSE)
    if (algo2=="tree")
      Yts[,m]<-pred(algo2,X,Y,X.ts,
                    mincut=1+m,classi=FALSE)
    if (algo2=="rf")
      Yts[,m]<-pred(algo2,X,Y,X.ts,
                    mtry=m,classi=FALSE)
  }
  
  
  ## W<-regrlin(Yp,Y[Ir],lambda=0.01)$beta.hat
  require(nnls)
  W<-nnls(cbind(numeric(NROW(Yp))+1,Yp),Y[Ir])$x
  
  
  if (is.na(sum(W)))
    W<-c(0,rep(1/M,M)) ##regrlin(Yp,Y[Ir],lambda=0.01)$beta.hat
  pred<-W[1]
  
  for (i in 1:m)
    pred<-pred+Yts[,i]*W[i+1]
  
  pred<-pmax(pmin(1,pred),0)
  
  prob<-cbind(1-pred,pred)
  colnames(prob)<-l.Y
  pred<-l.Y[apply(prob,1,which.max)]
  
  
  list(prob=prob,pred=pred)
  
}
