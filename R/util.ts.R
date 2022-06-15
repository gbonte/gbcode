#' Dataset A
#'@title A Santa Fe time series
#'@description chaotic time series from Santa Fe time series competition
#' @references \url{http://www-psych.stanford.edu/~andreas/Time-Series/SantaFe.html}
#' @references \url{http://www.amazon.com/Time-Series-Prediction-Forecasting-Understanding/dp/0201626020}
#' @name A
#' @docType data
#' @keywords data
#' @export
#' @examples
#' data(A)
#' plot(A[,1],type="l")
NULL

#' Dataset NN5
#'@title NN5 time series
#'@description  time series from NN5 time series competition
#' @references \url{http://www.neural-forecasting-competition.com/NN5/}
#' @references \url{https://www.researchgate.net/publication/222828050_Conditionally_dependent_strategies_for_multiple-step-ahead_prediction_in_local_learning}
#' @name NN5
#' @docType data
#' @keywords data
#' @export
#' @examples
#' data(NN5)
#' plot(NN5[,1],type="l")
NULL

#### SMAPE ####
#' Symmetric mean absolute percentage error
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error}
#' @title Symmetric mean absolute percentage error
SMAPE<-function(ts1,ts2,Cent=0,Sc=1){
  ts1<-Cent+ts1*Sc
  ts2<-Cent+ts2*Sc
  mean(abs(ts1-ts2)/((ts1+ts2)/2))*100
  
}

#### MASE ####
#' Mean Absolute scaled error
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{https://en.wikipedia.org/wiki/Mean_absolute_scaled_error}
#' @title Mean Absolute scaled error
MASE<-function(y,yhat){
  n<-length(y)
  e<-y-yhat
  q<-e/(mean(abs(diff(y))))
  
  mean(abs(q))
  
}

remNA<-function(TS){
  return(approx(seq(TS),TS,seq(TS))$y)
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

#### remNA ####
#' Remove NA from a time series by interpolation
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Remove NA from a time series by interpolation
#' @param TS: univariate time series
#' @export
#'
remNA<-function(TS){
  return(approx(seq(TS),TS,seq(TS))$y)
}

#### dist2 ####
#' Returns matrix of distances between two matrices with same number of columns
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Returns matrix of distances between two matrices with same number of columns
#' @param X1: matrix [N1,n]
#' @param X2: matrix [N2,n]
#' @return: matrix of euclidean distances [N1,N2]
#' @export
#'
dist2<-function(X1,X2){
  if (is.vector(X2))
    X2<-array(X2,c(1,length(X2)))
  N1<-NROW(X1)
  n<-NCOL(X1)
  n2<-NCOL(X2)
  N2<-NROW(X2)
  if (n != n2){
    cat("\n n=",n)
    cat("\n n2=",n2)
    stop('dist2 function: matrix sizes do not match.')
  }
  y<-array(0,c(N1,N2))
  
  if (n==1){
    for (i in 1:N1){
      x <- array(1,c(N2,1))%*%as.numeric(X1[i,])
      y[i,] <- abs(x-X2)
    }
  }else {
    if (N1<N2){
      for (i in 1:N1){
        x <- array(1,c(N2,1))%*%as.numeric(X1[i,])
        y[i,] <-apply(((x-X2)^2),1,sum)
        
      }
    }else {
      
      for (j in 1:N2){
        
        x <- array(1,c(N1,1))%*%as.numeric(X2[j,])
        y[,j] <-apply(((x-X1)^2),1,sum)
        
      }
      
    }
  }
  
  sqrt(y)
  
}

#### MakeEmbedded ####
#' Embed a multivariate time series in input output form
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Embedding of multivariate time series
#' @param ts: multivariate time series [no. observations,no. variates]
#' @param n [no.var]: vector of embedding orders
#' @param delay [no.var]: vector of delays
#' @param hor [length(w)]: vector of predicted horizons (hor=1 boils down to one-step-ahed prediction)
#' @param w: index of variables appearing in $out
#'
#' @return list with
#' \itemize{
#' \item{inp}: embedded inputs of all variables (n[i] columns for series ts[i])
#' \item{out}: outputs of variates whose index is in w}
#' @examples
#' TS<-array(1:25,c(5,5))
#' ## embeds a 5 variate time series with embedding orders equal to 2 for all variables for one-step ahead prediction
#' MakeEmbedded(TS,n=rep(2,5),delay=rep(0,5),hor=rep(1,5),w=1:5)
#'
#'
MakeEmbedded<-function(ts, n, delay,hor=1,w=1){
  
  no.data<-NROW(ts)
  no.var<-NCOL(ts)
  a<-NROW(n)
  b<-NCOL(n)
  if (a!=no.var)
    stop('Error in the size of embedding n')
  if (length(delay)!=no.var)
    stop('Error in the size of delay')
  if (length(hor)!=length(w))
    stop('Error in the size of horizon hor')
  N<-no.data-max(n)-max(delay)
  
  Input<-array(0,c(N,sum(n)))
  Output<-array(0,c(N,sum(hor)))
  
  for (i in 1:N) {
    for (j in 1:no.var) {
      k<-1:n[j]
      Input[i,sum(n[1:j-1])+k]<-ts[i+n[j]-k+max(n)-n[j]+max(delay)-delay[j],j]
      
      for (ww in 1:length(w)){
        if (ww==1)
          iw<-0
        else
          iw<-sum(hor[1:(ww-1)])
        
        Output[i,(iw+1):(sum(hor[1:ww]))]<-numeric(hor[ww])+NA
        M<-min(no.data,(i+max(n)+max(delay)+hor[ww]-1))
        
        
        Output[i,(iw+1):(iw+M-(i+max(n)+max(delay))+1)]<-ts[(i+max(n)+max(delay)):M,w[ww]]
        
      }
    }
    
  }
  
  if (NCOL(Output)>1){
    wna<-which(is.na(apply(Output,1,sum)))
    if (length(wna)>0){
      Input=Input[-wna,]
      Output=Output[-wna,]
    }
  }
  list(inp=Input,out=Output)
}


MakeEmbeddedrev<-function(ts, n, delay, hor = 1, w = 1) 
{
  no.data <- NROW(ts)
  no.var <- NCOL(ts)
  a <- NROW(n)
  b <- NCOL(n)
  if (a != no.var) 
    stop("Error in the size of embedding n")
  if (length(delay) != no.var) 
    stop("Error in the size of delay")
  if (length(hor) != length(w)) 
    stop("Error in the size of horizon hor")
  N <- no.data - max(n) - max(delay)
  Input <- array(0, c(N, sum(n)))
  Output <- array(0, c(N, sum(hor)))
  for (i in 1:N) {
    for (j in 1:no.var) {
      k <- 1:n[j]
      Input[i, sum(n[1:j - 1]) + k] <- rev(ts[i + n[j] - k + 
                                                max(n) - n[j] + max(delay) - delay[j], j])
      for (ww in 1:length(w)) {
        if (ww == 1) 
          iw <- 0
        else iw <- sum(hor[1:(ww - 1)])
        Output[i, (iw + 1):(sum(hor[1:ww]))] <- numeric(hor[ww]) + 
          NA
        M <- min(no.data, (i + max(n) + max(delay) + 
                             hor[ww] - 1))
        Output[i, (iw + 1):(iw + M - (i + max(n) + max(delay)) + 
                              1)] <- ts[(i + max(n) + max(delay)):M, w[ww]]
      }
    }
  }
  list(inp = Input, out = Output)
}


#### constloo ####
#' Leave-one-out Mean Squared Error of a weighted mean estimator
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @title Embedding of multivariate time series
#' @param x: observation vector
#' @param w: weight vector
#' @return: leave-one-out Mean Squared Error
#' @export
constloo<-function(x,w=rep(1,length(x))){
  if (length(x)!=length(w))
    stop("constloo function: lengths of vectors do not match")
  I<-which(!(is.na(x)))
  x<-x[I]
  w<-w[I]
  m<-sum(x*w)
  n<-length(x)
  eloo<-n*(x-m)/(n-1)
  max(1e-4,mean(eloo^2))
}


dfmldesign<-function(TS,m0,H,p0=2,CC=1,lambda=0,Lcv=10,Kmin=3,
                     models=c("stat_naive","lindirect")){
  
  n<-NCOL(TS)
  maxp=min(n,p0)
  maxm=m0
  nm=length(models)
  
  N<-NROW(TS)
  Xtr<-TS[1:min(N-H-1,floor(8*N/10)),]
  Ntr<-NROW(Xtr)
  Xts<-TS[(Ntr+1):N,]
  Nts<-NROW(Xts)
  
  C=cov(Xtr) 
  V=t(eigen(C,TRUE)$vectors[,1:maxp])
  Z=(TS%*%t(V))
  eps=1e-3
  
  Ehat<-array(0,c(CC,maxm,maxp,nm))
  for (mm in 1:nm){
    mod=models[mm]
    for (cc in 1:CC){
      for (m in 1:maxm){
        ZZ<-NULL
        for (s in seq(1,Nts-H-1,length.out=Lcv)){
          Zhat<-array(NA,c(H,maxp))
          muZ=mean(Z[1:(Ntr+s),1])
          stdZ=sd(Z[1:(Ntr+s),1])+eps
          sZ=(Z[1:(Ntr+s),1]-muZ)/stdZ
          Zhat[,1]=multiplestepAhead(sZ,n=m, H=H,method=mod,Kmin=Kmin,C=cc)
          Xts=X[(Ntr+s+1):(Ntr+s+H),]
          Xhat=(Zhat[,1]*stdZ+muZ)%*%array(V[1,],c(1,n))
          
          Ehat[cc,m,1,mm]<-Ehat[cc,m,1,mm]+mean(apply((Xts-Xhat)^2,2,mean))
          
          for (p in 2:maxp){
            muZ=mean(Z[1:(Ntr+s),p])
            stdZ=sd(Z[1:(Ntr+s),p])+eps
            sZ=(Z[1:(Ntr+s),p]-muZ)/stdZ
            Zhat[,p]=multiplestepAhead(sZ,n=m, H=H,method=mod,Kmin=Kmin,C=cc)
            Xhat=(Zhat[,1:p]*stdZ+muZ)%*%V[1:p,]
            Ehat[cc,m,p,mm]<-Ehat[cc,m,p,mm]+mean(apply((Xts-Xhat)^2,2,mean))
          } ## for p
          ZZ<-rbind(ZZ,Zhat)
        } ## for s
        if (lambda>0)
          for (p in 2:maxp){
            cZ=1-cor.prob(ZZ[,1:p])
            cZ= mean(c(cZ[upper.tri(cZ)]))
            Ehat[cc,m,p,mm]<-Ehat[cc,m,p,mm]/Lcv+lambda*cZ ## criterion for decorrelation of factor predictions 
          }
      } ## for m
    }
    cat(".")
  }
  
  Emin=min(Ehat)
  p0<-which.min(apply(Ehat,3,min))
  m<-which.min(apply(Ehat,2,min))
  cc<-which.min(apply(Ehat,1,min))
  mod=models[which.min(apply(Ehat,4,min))]
  C=cov(TS)
  V=t(eigen(C,TRUE)$vectors[,1:p0])
  
  return (list(p=p0,m=m,cc=cc,mod=mod,V=V[1:p0,]))
}


dfml<-function(TS,m,H,p0=3,cc=2,mod="stat_comb",
               Kmin=3,V=NULL,orth=FALSE){
  
  n<-NCOL(TS)
  p0=min(p0,n)
  N=NROW(TS)
  Zhat<-array(NA,c(H,p0))
  if (is.null(V)){
    C=cov(TS)
    V=t(eigen(C,TRUE)$vectors[,1:p0])
  }
  Ztr=TS%*%t(V)
  Zhat[,1]=multiplestepAhead(Ztr[,1],n=m, H=H,method=mod,Kmin=Kmin,C=cc)
  if (p0>1)
    for (p in 2:p0)
      Zhat[,p]=multiplestepAhead(Ztr[,p],n=m, H=H,method=mod,Kmin=Kmin,C=cc)
  if (p0>1)
    return(Zhat[,1:p0]%*%V[1:p0,])
  Xhat=Zhat[,1]%*%array(V[1:p0,],c(1,n))
  return(Xhat)
  
}


rnnpred<-function(TS,m,H){
  require(keras)
  n=NCOL(TS)
  
  M=MakeEmbeddedrev(TS,numeric(n)+m,numeric(n),numeric(n)+H,1:n)
  
  I=which(!is.na(apply(M$out,1,sum)))
  M$inp=M$inp[I,]
  M$out=M$out[I,]
  N=NROW(M$inp)
  n1=NCOL(M$inp)
  p<-NCOL(M$out)
  
  trainX=array(M$inp,c(NROW(M$inp),m,n))
  trainY=M$out
  
  model <- keras_model_sequential() %>%
    layer_simple_rnn(units = 6,input_shape=c(m,n))%>%
    layer_dropout(0.2) %>%
    layer_dense(ncol(trainY))
  
  model %>% compile(loss = 'mse',
                    optimizer = 'RMSprop',
                    metrics = c('accuracy'))
  
  
  model %>% fit(
    x = trainX, # sequence we're using for prediction                                                          
    y = trainY, # sequence we're predicting                                                                    
    epochs = 100, # how many times we'll look @ the whole dataset                                           
    validation_split = 0.1,verbose=0)
  
  lmodel=model
  q<-NULL
  D=0
  for (j in 1:n)
    q<-c(q,rev(TS[seq(N-D,N-m+1-D,by=-1),j]))
  
  Xts=array(q,c(1,1,length(q)))
  trainXts=array(Xts,c(1,m,n))
  Yhat<-array(NA,c(H,n))
  
  Yhat  <- lmodel%>% predict(trainXts,verbose=0)
  return(array(Yhat,c(H,n)))
  
  
}


lstmpred<-function(TS,m,H){
  require(keras)
  n=NCOL(TS)
  
  M=MakeEmbeddedrev(TS,numeric(n)+m,numeric(n),numeric(n)+H,1:n)
  
  I=which(!is.na(apply(M$out,1,sum)))
  M$inp=M$inp[I,]
  M$out=M$out[I,]
  N=NROW(M$inp)
  n1=NCOL(M$inp)
  p<-NCOL(M$out)
  
  trainX=array(M$inp,c(NROW(M$inp),m,n))
  trainY=M$out
  
  model <- keras_model_sequential() %>%
    layer_lstm(units = 6,input_shape=c(m,n))%>%
    layer_dropout(0.2) %>%
    layer_dense(ncol(trainY))
  
  model %>% compile(loss = 'mse',
                    optimizer = 'RMSprop',
                    metrics = c('accuracy'))
  
  
  model %>% fit(
    x = trainX, # sequence we're using for prediction                                                          
    y = trainY, # sequence we're predicting                                                                    
    epochs = 100, # how many times we'll look @ the whole dataset                                           
    validation_split = 0.1,verbose=0)
  
  lmodel=model
  q<-NULL
  D=0
  for (j in 1:n)
    q<-c(q,rev(TS[seq(N-D,N-m+1-D,by=-1),j]))
  
  Xts=array(q,c(1,1,length(q)))
  trainXts=array(Xts,c(1,m,n))
  Yhat<-array(NA,c(H,n))
  
  Yhat  <- lmodel%>% predict(trainXts,verbose=0)
  return(array(Yhat,c(H,n)))
  
  
}


multifs<-function(TS,n,H,mod="rf"){
  
  m=NCOL(TS)
  N=NROW(TS)
  M=MakeEmbedded(TS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  Yhat<-array(NA,c(H,m))
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,TS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  for (i in 1:m)
    for (h in 1:H){
      Y=YY[,(i-1)*H+h]
      fs<-mrmr(XX,Y,min(NCOL(XX)-1,5))
      Yhat[h,i]=pred(mod,XX[,fs],Y,Xts[,fs],class=FALSE)
      
    }
  return(Yhat)
}
