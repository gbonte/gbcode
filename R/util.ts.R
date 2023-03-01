
#### SMAPE #### 
#' Symmetric mean absolute percentage error
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error}
#' @param ts1: series 1
#' @param ts2: series 2
#' @param Cent: center
#' @param Sc: scale factor
#' 
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
#### remNA ####
#' Remove NA from a multivariate time series by interpolation
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{mlg.ulb.ac.be}
#' @title Remove NA from a time series by interpolation
#' @param TS: univariate/multivariate time series
#' @export
#'
remNA<-function(TS){
  if (is.vector(TS)){
    TS2=approx(seq(TS),TS,seq(TS))$y
    w<-which(is.na(TS2))
    if (length(w)>0)
      TS2[w]=mean(TS2,na.rm=TRUE)
    return(TS2)
  }
  TS2=TS*0
  for (i in 1:NCOL(TS)){
    TS2[,i]=approx(seq(TS[,i]),TS[,i],seq(TS[,i]))$y
    w<-which(is.na(TS2[,i]))
    if (length(w)>0)
      TS2[w,i]=mean(TS2[,i],na.rm=TRUE)
  }
  return(TS2)
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
#' 
#' @export
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
  if (sum(n)==1)
    Input=array(Input,c(length(Input),1))
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


oldfmldesign<-function(TS,m0,H,p0=2,Lcv=3,
                     models=c("stat_naive","lindirect"),...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  n<-NCOL(TS)  ## number of series
  maxp=min(n,p0)  ## max no PC components
  maxm=m0 ## max autoregressive order
  nm=length(models) ## number of forecasting models
  
  N<-NROW(TS)
  Xtr<-TS[1:min(N-H-1,floor(9*N/10)),]
  Ntr<-NROW(Xtr)
  Xts<-TS[(Ntr+1):N,]
  Nts<-NROW(Xts)
  
  C=cov(Xtr) 
  V=t(eigen(C,TRUE)$vectors[,1:maxp])
  Z=(TS%*%t(V))
  eps=1e-4
  
  Ehat<-array(0,c(maxm,maxp,nm))
  Eref<-0
  for (mm in 1:nm){ ## loop over forecasting model
    mod=models[mm]
    
    for (m in 1:maxm){ ## loop over autoregressive order
      
      for (s in round(seq(0,Nts-H-1,length.out=Lcv))){
        XXts=TS[(Ntr+s+1):(Ntr+s+H),]
        
        if (m==1 & mm==1){
          Xref=NULL
          for (j in 1:n)
            Xref=cbind(Xref,multiplestepAhead(TS[1:(Ntr+s),j],m,H, method="stat_comb"))
          Eref<-Eref+mean(apply((XXts-Xref)^2,2,mean))
        }
        if (mod=="vars"){
          Xhat=VARspred(TS[1:(Ntr+s),],m,H,method=1)
          Ehat[m,1,mm]<-Ehat[m,1,mm]+mean(apply((XXts-Xhat)^2,2,mean))
        }
        Zhat<-array(NA,c(H,maxp))
        muZ=mean(Z[1:(Ntr+s),1])
        stdZ=sd(Z[1:(Ntr+s),1])+eps
        sZ=(Z[1:(Ntr+s),1]-muZ)/stdZ
        if (mod=="multifs"){
          Zhat[,1]=multiplestepAhead(sZ,n=m, H=H,method="stat_comb")
          
          ##Zhat[,1]=multifs2(array(sZ,c(length(sZ),1)),n=m, H=H)
          Zhat[,1]=Zhat[,1]*stdZ+muZ
          
          Xhat=(Zhat[,1])%*%array(V[1,],c(1,n))
          Ehat[m,1,mm]<-Ehat[m,1,mm]+mean(apply((XXts-Xhat)^2,2,mean))
        }
        
        
        if (!is.element(mod,c("vars","multifs"))){
          
          Zhat[,1]=multiplestepAhead(sZ,n=m, H=H,method=mod,...)
          Zhat[,1]=Zhat[,1]*stdZ+muZ
          
          Xhat=(Zhat[,1])%*%array(V[1,],c(1,n))
          
          Ehat[m,1,mm]<-Ehat[m,1,mm]+mean(apply((XXts-Xhat)^2,2,mean))
        }
        for (p in 2:maxp){ ## loop over number of Pcomponents
          muZ=mean(Z[1:(Ntr+s),p])
          stdZ=sd(Z[1:(Ntr+s),p])+eps
          sZ=(Z[1:(Ntr+s),p]-muZ)/stdZ
          if (mod=="vars"){
            Xhat=VARspred(TS[1:(Ntr+s),],m,H,method=p)
            Ehat[m,p,mm]<-Ehat[m,p,mm]+mean(apply((XXts-Xhat)^2,2,mean))
          }
          if (mod=="multifs"){
            Xhat=multiridge(Z[1:(Ntr+s),1:p],m,H,MIMO=TRUE,direct=FALSE,preq=FALSE,...)$Yhat
            Xhat=Xhat%*%V[1:p,]
            #Xhat=multiml(Z[1:(Ntr+s),1:p],m,H,learner="lin")%*%V[1:p,]
            Ehat[m,p,mm]<-Ehat[m,p,mm]+mean(apply((XXts-Xhat)^2,2,mean))
          }
          
          
          if (!is.element(mod,c("vars","multifs"))){
            Zhat[,p]=multiplestepAhead(sZ,n=m, H=H,method=mod,...)
            Zhat[,p]=Zhat[,p]*stdZ+muZ
            Xhat=Zhat[,1:p]%*%V[1:p,]
            Ehat[m,p,mm]<-Ehat[m,p,mm]+mean(apply((XXts-Xhat)^2,2,mean))
          }
        } ## for p
        
      } ## for s
      
    } ## for m
  }
  
  Emin=min(Ehat)
  
  ## comparison with univariate model
  if (quantile(Ehat,0.05)<Eref){
    bestp<-which.min(apply(Ehat,2,min))
    bestm<-which.min(apply(Ehat,1,min))
    bestmod=models[which.min(apply(Ehat,3,min))]
  } else {
    bestp<--1
    bestm<-0
    bestmod="uni"
  }
  return (list(p=bestp,m=bestm,mod=bestmod))
}

dfmldesign<-function(TS,n0,H,p0=2,Lcv=3,
                     models=c("stat_naive","lindirect"),...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  m<-NCOL(TS)  ## number of series
  maxp=min(m,p0)  ## max no PC components
  maxn=n0 ## max autoregressive order
  nmods=length(models) ## number of forecasting models
  
  N<-NROW(TS)
  Xtr<-TS[1:min(N-H-1,floor(9*N/10)),]
  Ntr<-NROW(Xtr)
  Xts<-TS[(Ntr+1):N,]
  Nts<-NROW(Xts)
  
  C=cov(Xtr) 
  V=t(eigen(C,TRUE)$vectors[,1:maxp])
  Z=(TS%*%t(V))
  eps=1e-4
  
  Ehat<-array(0,c(maxn,maxp,nmods))
  Eref<-0
  for (mm in 1:nmods){ ## loop over forecasting model
    mod=models[mm]
    
    for (nn in 1:maxn){ ## loop over autoregressive order
      
      for (s in round(seq(0,Nts-H-1,length.out=Lcv))){
        XXts=TS[(Ntr+s+1):(Ntr+s+H),]
        
        if (nn==1 & mm==1){
          Xref=NULL
          for (j in 1:m)
            Xref=cbind(Xref,multiplestepAhead(TS[1:(Ntr+s),j],nn,H, method="stat_comb"))
          Eref<-Eref+mean(apply((XXts-Xref)^2,2,mean))
        }
        if (mod=="vars"){
          Xhat=VARspred(TS[1:(Ntr+s),],nn,H,method=1)
          Ehat[nn,1,mm]<-Ehat[nn,1,mm]+mean(apply((XXts-Xhat)^2,2,mean))
        }
        Zhat<-array(NA,c(H,maxp))
        muZ=mean(Z[1:(Ntr+s),1])
        stdZ=sd(Z[1:(Ntr+s),1])+eps
        sZ=(Z[1:(Ntr+s),1]-muZ)/stdZ
        if (mod=="MIMO"){
          Zhat[,1]=multiplestepAhead(sZ,n=nn, H=H,method="mimo_rr")
          
          ##Zhat[,1]=multifs2(array(sZ,c(length(sZ),1)),n=m, H=H)
          Zhat[,1]=Zhat[,1]*stdZ+muZ
          
          Xhat=(Zhat[,1])%*%array(V[1,],c(1,m))
          Ehat[nn,1,mm]<-Ehat[nn,1,mm]+mean(apply((XXts-Xhat)^2,2,mean))
        }
        
        if (!is.element(mod,c("vars","MIMO"))){
          
          Zhat[,1]=multiplestepAhead(sZ,n=nn, H=H,method=mod,...)
          Zhat[,1]=Zhat[,1]*stdZ+muZ
          
          Xhat=(Zhat[,1])%*%array(V[1,],c(1,m))
          
          Ehat[nn,1,mm]<-Ehat[nn,1,mm]+mean(apply((XXts-Xhat)^2,2,mean))
        }
        for (p in 2:maxp){ ## loop over number of Pcomponents
          muZ=mean(Z[1:(Ntr+s),p])
          stdZ=sd(Z[1:(Ntr+s),p])+eps
          sZ=(Z[1:(Ntr+s),p]-muZ)/stdZ
          if (mod=="vars"){
            Xhat=VARspred(TS[1:(Ntr+s),],nn,H,method=p)
            Ehat[nn,p,mm]<-Ehat[nn,p,mm]+mean(apply((XXts-Xhat)^2,2,mean))
          }
          if (mod=="MIMO"){
            Xhat=multiridge(Z[1:(Ntr+s),1:p],nn,H,MIMO=TRUE,direct=FALSE,preq=FALSE,...)$Yhat
            Xhat=Xhat%*%V[1:p,]
            #Xhat=multiml(Z[1:(Ntr+s),1:p],m,H,learner="lin")%*%V[1:p,]
            Ehat[nn,p,mm]<-Ehat[nn,p,mm]+mean(apply((XXts-Xhat)^2,2,mean))
          }
          
          
          if (!is.element(mod,c("vars","MIMO"))){
            Zhat[,p]=multiplestepAhead(sZ,n=nn, H=H,method=mod,...)
            Zhat[,p]=Zhat[,p]*stdZ+muZ
            Xhat=Zhat[,1:p]%*%V[1:p,]
            Ehat[nn,p,mm]<-Ehat[nn,p,mm]+mean(apply((XXts-Xhat)^2,2,mean))
          }
        } ## for p
        
      } ## for s
      
    } ## for nn
  }
  
  Emin=min(Ehat)
  
  ## comparison with univariate model
  if (quantile(Ehat,0.05)<Eref){
    bestp<-which.min(apply(Ehat,2,min))
    bestn<-which.min(apply(Ehat,1,min))
    bestmod=models[which.min(apply(Ehat,3,min))]
  } else {
    bestp<--1
    bestn<-0
    bestmod="uni"
  }
  return (list(p=bestp,n=bestn,mod=bestmod))
}

dfml<-function(TS,n,H,p0=3,dfmod="lindirect",...){
  ## n: autoregressive order
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  m<-NCOL(TS)
  p0=min(p0,m)
  
  if (p0<0){
    Xhat=NULL
    for (j in 1:m)
      Xhat=cbind(Xhat,multiplestepAhead(TS[,j],n,H, method="stat_comb"))
    return(Xhat)
  }
  if (dfmod=="vars") 
    return(VARspred(TS,n,H,method=p0))
  
  N=NROW(TS)
  Zhat<-array(NA,c(H,p0))
  C=cov(TS)
  V=t(eigen(C,TRUE)$vectors[,1:p0])
  eps=1e-4
  Ztr=TS%*%t(V)
  
  if (dfmod=="MIMO"){
    if (p0==1){
      Zhat=multiplestepAhead(Ztr[,1],n=n, H=H,method="mimo_rr")
      return(Zhat%*%array(V[1:p0,],c(1,m)))
    }
    ##Zhat=multiml(Ztr[,1:p0],n,H,learner="lin")
    Zhat=MmultiplestepAhead(Ztr[,1:p0],n,H,multi="MIMO_rr")
    return(Zhat%*%V[1:p0,])
  }
  
  muZ=mean(Ztr[,1])
  stdZ=sd(Ztr[,1])+eps
  sZ=(Ztr[,1]-muZ)/stdZ
  Zhat[,1]=multiplestepAhead(sZ,n=n, H=H,method=dfmod,...)
  Zhat[,1]=(Zhat[,1]*stdZ+muZ)
  if (p0>1){
    for (p in 2:p0){
      muZ=mean(Ztr[,p])
      stdZ=sd(Ztr[,p])+eps
      sZ=(Ztr[,p]-muZ)/stdZ
      Zhat[,p]=multiplestepAhead(sZ,n=n, H=H,method=dfmod,...)
      Zhat[,p]=(Zhat[,p]*stdZ+muZ)
    }
    
    Xhat=Zhat[,1:p0]%*%V[1:p0,]
    return(Xhat)
  }
  Xhat=Zhat[,1]%*%array(V[1:p0,],c(1,m))
  return(Xhat)
  
}

kfml<-function(TS,n,H,p0=3,dfmod="lindirect",adaptive=FALSE,...){
  
  ## n: autoregressive order
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  m<-NCOL(TS)
  p0=min(p0,m)
  
  N=NROW(TS)
  Zhat<-array(NA,c(H,p0))
  C=cov(TS)
  V=t(eigen(C,TRUE)$vectors[,1:p0])
  eps=1e-4
  Ztr=TS%*%t(V)
  A=V[1:p0,]
  
  Phi=array(0,c(p0,p0))
  sdw=1
  Q=sdw*diag(p0)
  R=sdw*diag(m)
  Sigma0=sdw*diag(p0)
  lambda=0.001
  R<-chol(cov(TS-Ztr[,1:p0]%*%V[1:p0,])+lambda*diag(m))
  
  E<-NULL
  for (j in 1:p0){
    X=Ztr[1:(N-1),]
    Y=Ztr[2:N,j]
    Phi[j,]=solve(t(X)%*%X)%*%t(X)%*%Y
    E=cbind(E,(Y-X%*%array(Phi[j,],c(p0,1))))
  }
  Q=chol(cov(E)+lambda*diag(p0))
  
  
  mu0=array(Ztr[1,],c(p0,1))
  if (adaptive){
    invisible(capture.output(E<-EM(TS,A=t(A),Q=Q,R=R, Phi=Phi,
                                   mu0=mu0,
                                   Sigma0=Sigma0)))
    
    
    mu=E$mu0
    Sigma0=E$Sigma0
    Phi=E$Phi
    Q=E$Q
    R=E$R
  }
  run = Kfilter(TS, A=t(A), mu0, Sigma0, Phi, Q, R)
  Ztr=NULL
  for (p in 1:p0)
    Ztr=cbind(Ztr,run$Xf[p,,])
  
  if (FALSE){
    for (p in 1:p0){
      muZ=mean(Ztr[,p])
      stdZ=sd(Ztr[,p])+eps
      sZ=(Ztr[,p]-muZ)/stdZ
      Zhat[,p]=multiplestepAhead(sZ,n=n, H=H,method=dfmod,...)
      Zhat[,p]=(Zhat[,p]*stdZ+muZ)
    }
  }
  
  Zhat=multifs2(Ztr[,1:p0],m,H)
  Xhat=Zhat[,1:p0]%*%V[1:p0,]
  
  return(Xhat)
  
  
}


VARspred<-function(TS,n,H,method=1,...){
  n=NCOL(TS)
  colnames(TS)=1:n
  if (method>4)
    method=method%%4
  if (method==1)
    V=VARshrink(TS, p = n, type = "const", method = "ridge")
  if (method==2)
    V=VARshrink(TS, p = n, type = "none", method = "ridge")
  if (method==3)
    V=VARshrink(TS, p = n, type = "const", method = "ns")
  if (method==4)
    V=VARshrink(TS, p = n, type = "none", method = "ns")
  if (method==5)
    V=VARshrink(TS, p = n, type = "const", method = "kcv")
  P=predict(V,n.ahead=H)
  Yhat=NULL
  for (i in 1:n)
    Yhat=cbind(Yhat,P$fcst[[i]][,1])
  return(Yhat)
}

nbeatspred<-function(TS,H=10,n=4,nepochs=5){
  require(modeltime.gluonts)
  N=NROW(TS)
  DatesX=as.Date(1:(N+H),origin="2000/1/2")
  
  X<-data.frame(DatesX[1:N],
                rep("id",N),
                TS)
  
  colnames(X)=c("date","id","value")
  
  
  
  invisible(capture.output(model_fit <- nbeats(
    id                    = "id",
    freq                  = "D",
    prediction_length     = H,
    lookback_length       = n,
    epochs                = nepochs
  ) %>%
    set_engine("gluonts_nbeats") %>%
    fit(value ~ date+id, X)))
  
  # ---- FORECAST ----
  new_data <- tibble(
    id   = factor("id"),
    date = DatesX[(N+1):(N+H)]
  )
  
  P<-predict(model_fit, new_data)
  
  Yhat<-unlist(P%>%
                 dplyr::select(".pred"))
  return(Yhat)
}

nbeatsenspred<-function(TS,H=10,n=4,nepochs=5){
  require(modeltime.gluonts)
  N=NROW(TS)
  DatesX=as.Date(1:(N+H),origin="2000/1/2")
  
  X<-data.frame(DatesX[1:N],
                rep("id",N),
                TS)
  
  colnames(X)=c("date","id","value")
  
  
  
  invisible(capture.output(model_fit <- nbeats(
    id                    = "id",
    freq                  = "D",
    prediction_length     = H,
    lookback_length       = n,
    epochs                = nepochs
  ) %>%
    set_engine("gluonts_nbeats_ensemble") %>%
    fit(value ~ date+id, X)))
  
  # ---- FORECAST ----
  new_data <- tibble(
    id   = factor("id"),
    date = DatesX[(N+1):(N+H)]
  )
  
  P<-predict(model_fit, new_data)
  
  Yhat<-unlist(P%>%
                 dplyr::select(".pred"))
  return(Yhat)
}

gluonpred<-function(TS,H=10,n=4,nepochs=5){
  require(modeltime.gluonts)
  N=NROW(TS)
  DatesX=as.Date(1:(N+H),origin="2000/1/2")
  
  X<-data.frame(DatesX[1:N],
                rep("id",N),
                TS)
  
  colnames(X)=c("date","id","value")
  invisible(capture.output(model_fit_deepar <- deep_ar(
    id                    = "id",
    freq                  = "D",
    prediction_length     = H,
    lookback_length       = n,
    epochs                = nepochs
  ) %>%
    set_engine("gluonts_deepar") %>%
    fit(value ~ date+id, X)))
  
  # ---- FORECAST ----
  new_data <- tibble(
    id   = factor("id"),
    date = DatesX[(N+1):(N+H)]
  )
  
  P<-predict(model_fit_deepar, new_data)
  
  Yhat<-unlist(P%>%
                 dplyr::select(".pred"))
  return(Yhat)
}

pylstmpred<-function(TS,H,n,nepochs=10,...){
  sTS<-scale(TS)
  N=NROW(TS)
  m=NCOL(sTS)
  M<-MakeEmbedded(array(sTS[1:N,],c(N,m)),n=numeric(m)+n,numeric(m),hor=numeric(m)+H,w=1:m)
  
  X<-array(M$inp,c(NROW(M$inp),n,m))
  Y=M$out #array(M$out,c(NROW(M$out),H,m))
  
  Xts <- NULL
  for (j in 1:m)
    Xts<-c(Xts,sTS[seq(N,N-n+1,by=-1),j])
  Xts<-array(Xts,c(1,n,m))
  
  
  Nts=NROW(Xts)
  pyX<<-X;   pyXts<<-Xts;   pyY<<-Y;   pyN<<-NROW(X);      
  pyNts<<-Nts;  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;
  
  plearn<<-"lstm_ts2"
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  Yhat
}


pylstmpredgpt<-function(TS,H,n,nepochs=50,nunits=20,hyper=TRUE,...){
  
  m<-NCOL(TS)
  pyTS<<-cbind(TS); pyY<<-TS;       
  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;
  
  if (hyper)
    plearn<<-"lstm_gpt_hyper"
  else
    plearn<<-"lstm_gpt"
  
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  
  Yhat
}

pyrnnpredgpt<-function(TS,H,n,nepochs=50,nunits=20,hyper=TRUE,...){
  
  m<-NCOL(TS)
  pyTS<<-cbind(TS); pyY<<-TS;         
  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;
  
  if (hyper)
    plearn<<-"rnn_gpt_hyper"
  else
    plearn<<-"rnn_gpt"
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  
  Yhat
}

pytransfpredgpt<-function(TS,H,n,nepochs=50,nunits=20,hyper=TRUE,...){
  
  m<-NCOL(TS)
  pyTS<<-cbind(TS); pyY<<-TS;         
  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;
  
  
  if (hyper)
    plearn<<-"transformer_gpt_hyper"
  else
    plearn<<-"transformer_gpt"
  
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  
  Yhat
}


pytorchnbeats<-function(TS,H,n,nepochs=50,nunits=20,hyper=TRUE,...){
  
  m<-NCOL(TS)
  pyTS<<-cbind(TS); pyY<<-TS;         
  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;
  
  if (m>1)
    stop("pytorchnbeats only for univariate")
  
 
  plearn<<-"nbeats_pytorch"
  
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  
  Yhat
}

pytorchndeepar<-function(TS,H,n,nepochs=50,nunits=20,hyper=TRUE,...){
  
  m<-NCOL(TS)
  pyTS<<-cbind(TS); pyY<<-TS;         
  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;
  
  if (m>1)
    stop("pytorchdeepar only for univariate")
  
  
  plearn<<-"deepar_pytorch"
  
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  
  Yhat
}

pytorchntft<-function(TS,H,n,nepochs=50,nunits=20,hyper=TRUE,...){
  
  m<-NCOL(TS)
  pyTS<<-cbind(TS); pyY<<-TS;         
  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;
  
  if (m>1)
    stop("pytorchtft only for univariate")
  
  
  plearn<<-"tft_pytorch"
  
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  
  Yhat
}

dartstft<-function(TS,H,n,nepochs=5,nunits=20,hyper=TRUE,...){
  
  m<-NCOL(TS)
  pyTS<<-cbind(TS); pyY<<-TS;         
  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;
  
  
  plearn<<-"darts_tft"
  
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  
  Yhat
}

dartsnbeats<-function(TS,H,n,nepochs=5,nunits=20,hyper=TRUE,...){
  
  m<-NCOL(TS)
  pyTS<<-cbind(TS); pyY<<-TS;         
  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;
  
  
  plearn<<-"darts_nbeats"
  
  py_run_file(system.file("python", "libpy.py", package = "gbcode"))
  Yhat=array(py$yhat,c(H,m))
  
  Yhat
}

# for each series and for each horizon it selects relevant features and
# return the forecast
multifs<-function(TS,n,H,w=NULL,nfs=3,mod,...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  m=NCOL(TS)
  N=NROW(TS)
  sTS=scale(TS)
  if (is.null(w))
    w=1:m
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  Yhat<-array(NA,c(H,length(w)))
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  for (i in 1:length(w))
    for (h in 1:H){
      Y=YY[,(w[i]-1)*H+h]
      fs<-mrmr(XX,Y,min(NCOL(XX)-1,nfs))
      
      Yhat[h,i]=pred(mod,XX[,fs],Y,array(Xts[,fs],c(1,length(fs))),
                     class=FALSE)
      
    }
  for (i in 1:length(w))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  return(Yhat)
}

multipls<-function(TS,n,H,w=NULL,...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  m=NCOL(TS)
  N=NROW(TS)
  sTS=scale(TS)
  if (is.null(w))
    w=1:m
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  q<-1
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  N<-NROW(XX) # number training data
  nn<-NCOL(XX) # number input variables
  p<-nn+1
  XX<-cbind(array(1,c(N,1)),as.matrix(XX))
  DXX<-data.frame(XX)
  names(DXX)<-as.character(1:NCOL(XX))
  DXts<-data.frame(Xts)
  names(DXts)<-as.character(1:NCOL(Xts))
  
  
  MV<-plsr(YY~.,data=DXX,validation="CV",segments=3)
  LP<-predict(MV,newdata=DXts)
  nc<-which.min(apply(MV$validation$PRESS,2,mean,na.rm=T))
  
  Yhat<-c(LP[,,nc])
  Yhat=array(Yhat,c(H,m))
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  
  
  return(Yhat)
}

multirr<-function(TS,n,H,w=NULL,nfold = 10,...){
  require(rrpack)
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  m=NCOL(TS)
  N=NROW(TS)
  sTS=scale(TS)
  if (is.null(w))
    w=1:m
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  q<-1
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  N<-NROW(XX) # number training data
  nn<-NCOL(XX) # number input variables
  p<-nn+1
  XX<-cbind(array(1,c(N,1)),as.matrix(XX))
  DXX<-data.frame(XX)
  names(DXX)<-as.character(1:NCOL(XX))
  DXts<-data.frame(Xts)
  names(DXts)<-as.character(1:NCOL(Xts))
  
  rfit <- tryCatch(
    {
      cv.rrr(YY, XX, nfold = 10,maxrank=min(c(NCOL(YY),NCOL(XX),5)))
    },
    error = function(e){
      rrs.fit(YY, XX,nrank=3)
    }
  )
  
  
  Yhat<-Xts%*%rfit$coef
  Yhat=array(Yhat,c(H,m))
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  
  
  return(Yhat)
}

svdcca<-function(X,Y){
  require(expm)
  require(corpcor)
  if (NROW(X)!= NROW(Y) | NCOL(Y)<=1)
    stop("error in svdcca")
  
  SigmaX=array(cov.shrink(X,verbose=FALSE),c(NCOL(X),NCOL(X)))
  SigmaY=array(cov.shrink(Y,verbose=FALSE),c(NCOL(Y),NCOL(Y)))
  modSX=-0.5*logm(SigmaX)
  modSY=-0.5*logm(SigmaY)
  SigmaYX=cov(Y,X)
  SS<- tryCatch(
    {
      eSY=expm(modSY)
      eSX=expm(modSX)
      svd(eSY%*%SigmaYX%*%eSX)
    },
    error = function(e){
      browser()
      eSY=expm(modSY,method="Ward77",tol=0.1)
      eSX=expm(modSX,method="Ward77",tol=0.1)
      svd(eSY%*%SigmaYX%*%eSX,nu=2,nv=2)
    }
  )
  #a1=X%*%expm(modSX)%*%SS$v[,1]
  #b1=Y%*%expm(modSY)%*%SS$u[,1]
  
  list(U=(SS$u),V=SS$v,rho2=SS$d)
}

multicca<-function(TS,n,H,nfs=10,minLambda=0.1,
                   maxLambda=1000,nLambdas=25,...){
  
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  m=NCOL(TS)
  N=NROW(TS)
  sTS=scale(TS)
  
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  N<-NROW(XX) # number training data
  nn<-NCOL(XX) # number input variables
  
  
  sYY<-scale(YY)
  sXX<-scale(rbind(XX,Xts))
  sXts<-sXX[NROW(sXX),]
  sXX<-sXX[1:(NROW(sXX)-1),]
  SVDCCA<-svdcca(sXX, sYY)
  maxnfs=max(2,min(c(NCOL(sXX),NCOL(sYY),length(which(SVDCCA$rho2>0.1)))))
  minMSE=Inf
  for (rankU in round(seq(2,maxnfs,length.out=3)))
    for (rankV in round(seq(2,maxnfs,length.out=3))){
      Ur=SVDCCA$U[,1:rankU]
      YYc<-sYY%*%Ur
      Vr=SVDCCA$V[,1:rankV]
      XXc<-sXX%*%Vr
      sXtsc<-sXts%*%Vr
      XX1<-cbind(array(1,c(N,1)),as.matrix(XXc))
      p<-NCOL(XX1)
      XXX<-t(XX1)%*%(XX1)
      
      for (lambdah in seq(0.1,100,length.out=nLambdas)){
        H1<-ginv(XXX+lambdah*diag(p))
        beta.hat<-H1%*%t(XX1)%*%YYc
        HH=XX1%*%H1%*%t(XX1)
        
        Y.hat<-HH%*%YYc%*%t(Ur)
        e<-sYY-Y.hat
        e.loo<-e
        
        for (j in 1:NCOL(e)){
          e.loo[,j]<-e[,j]/pmax(1-diag(HH),0.01)
          w.na<-which(is.na(e.loo[,j]))
          if (length(w.na)>0)
            e.loo[w.na,j]=1
        }
        
        MSE.loo <-mean(e.loo^2,na.rm=TRUE )
        if (MSE.loo<minMSE){
          minMSE<-MSE.loo
          sYhat=array(c(1,sXtsc)%*%beta.hat,c(1,NCOL(YYc)))%*%t(Ur)
          
        }
      }
    }
  
  for (i in 1:NCOL(sYhat))
    sYhat[,i]=sYhat[,i]*attr(sYY,'scaled:scale')[i]+attr(sYY,'scaled:center')[i]
  Yhat=array(sYhat,c(H,m))
  
  
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  
  
  return(Yhat)
}

rls<-function(x,y,t,P,mu=1){
  x=rbind(x)
  P.new <-(P-(P%*%t(x)%*%x%*%P)/as.numeric(1+x%*%P%*%t(x)))/mu
  ga <- P.new%*%t(x)
  
  epsi <- y-x%*%t
  
  t.new<-t+ga%*%as.numeric(epsi)
  list(t.new,P.new,mean(epsi^2))
}



multipreq<-function(XX,YY,H=NULL,minLambda=0.1,
                    maxLambda=100,nLambda=25){
  n<-NCOL(XX)
  N<-NROW(XX)
  m<-NCOL(YY)   
  
  minMSE<-Inf
  for (lambdah in seq(minLambda,maxLambda,length.out=nLambda)){
    t<-rbind(numeric(m)+1,array(0,c(n,m)))
    P<-lambdah*diag(n+1)
    mu<-1
    E<-NULL
    for (i in 1:N){
      rls.step<-rls(c(1, XX[i,]),YY[i,],t,P,mu)
      t<-rls.step[[1]]
      P<-rls.step[[2]]
      if (i>N/3)
        E<-c(E,rls.step[[3]])
    }
    if (mean(E)<minMSE){
      minMSE=mean(E)
      lambda=lambdah
      betahat=t
    }
  }
  return(list(beta.hat=betahat,minMSE=minMSE,lambda=lambda))
}
mlin<-function(XX,YY,H=NULL,minLambda=0.1,
               maxLambda=5000,nLambdas=25,maha=FALSE){
  N<-NROW(XX) # number training data
  nn<-NCOL(XX) # number input variables
  m<-NCOL(YY)
  p<-nn+1
  XX<-cbind(array(1,c(N,1)),as.matrix(XX))
  XXX<-t(XX)%*%(XX)
  
  min.MSE.loo<-Inf
  bestlambdaY<-numeric(m)
  min.MSE.looY<-numeric(m)+Inf
  for (lambdah in seq(minLambda,maxLambda,length.out=nLambdas)){
    H1<- tryCatch(
      {
        ginv(XXX+lambdah*diag(p))
      },
      error = function(e){
        ginv(XXX+100*lambdah*diag(p))
      }
    )
    
    
    beta.hat<-H1%*%t(XX)%*%YY
    HH=XX%*%H1%*%t(XX)
    
    Y.hat<-HH%*%YY
    e<-YY-Y.hat
    e.loo<-e
    Y.loo=YY
    
    for (j in 1:NCOL(e)){
      e.loo[,j]<-e[,j]/pmax(1-diag(HH),0.01)
      w.na<-which(is.na(e.loo[,j]))
      if (length(w.na)>0)
        e.loo[w.na,j]=1
      Y.loo[,j]=Y.loo[,j]-e.loo[,j]
      
    }
    uMSE.loo<-NULL
    uSDSE.loo<-NULL
    if (!is.null(H))
      for (i in 1:(NCOL(YY)/H)){
        uMSE.loo<-c(uMSE.loo,mean(e.loo[,((i-1)*H+1):(i*H)]^2))
        uSDSE.loo<-c(uSDSE.loo,sd(e.loo[,((i-1)*H+1):(i*H)]^2))
      }
    MSE.looY<-apply(e.loo^2,2,mean)
    if (!maha){
      MSE.loo<-mean(e.loo^2,na.rm=TRUE )
    }else {
      require(corpcor)
      invisible (capture.output(S<-invcov.shrink(YY,verbose=FALSE)))
      MSE.loo<-mean(e.loo%*%S%*%t(e.loo))
    }  
    ## correlation between predicted sequence and real sequence
    #require(shapes)
    #MSE.loo<-MSE.loo+distcov(cov(YY),cov(Y.loo),"ProcrustesShape")
    
    if (is.na(MSE.loo))
      browser()
    if (MSE.loo<min.MSE.loo){
      lambda<-lambdah
      min.MSE.loo<-MSE.loo
      min.uMSE.loo<-uMSE.loo
      min.uSDSE.loo<-uSDSE.loo
    }
    for (j in 1:m)
      if (MSE.looY[j]<min.MSE.looY[j]){
        bestlambdaY[j]<-lambdah
        min.MSE.looY[j]<-MSE.looY[j]
        
      }
    
    
  }
  H1<- tryCatch(
    {
      ginv(XXX+lambda*diag(p))
    },
    error = function(e){
      ginv(XXX+100*lambda*diag(p))
    }
  )
  beta.hat<-H1%*%t(XX)%*%YY
  beta.hatY<-NULL
  for (j in 1:m){
    beta.hatY<-cbind(beta.hatY,ginv(XXX+bestlambdaY[j]*diag(p))%*%t(XX)%*%YY[,j])
    
  }
  
  return(list(beta.hat=beta.hat,minMSE=min.MSE.loo,
              minuMSE=min.uMSE.loo-min.uSDSE.loo,lambda=lambda,beta.hatY=beta.hatY))
}


whiteMaha<-function(X){
  N=NROW(X)
  n=NCOL(X)
  meanX=array(apply(X,2,mean),c(1,n))
  SigmaX=cov(X)
  S=svd(SigmaX)
  W=S$u%*%diag(1/sqrt(S$d))%*%t(S$v)
  ## whitening matrix W= \Sigma^{-1/2}
  ## satisfying W^T *W =\Sigma^{-1}
  ##
  IW=S$u%*%diag(sqrt(S$d))%*%t(S$v)
  
  #W=diag(1/sqrt(S$d))%*%t(S$v)
  #IW=S$v%*%diag(sqrt(S$d))
  
  oneN=array(1,c(N,1))
  Y=(X-oneN%*%meanX)%*%W
  list(sX=Y,mX=meanX,Isigma=IW)
}

colourMaha<-function(Y,meanX,IsigmaX){
  ## colouring transformation
  N=NROW(Y)
  n=NCOL(Y)
  meanX=array(meanX,c(1,n))
  oneN=array(1,c(N,1))
  
  return(Y%*%IsigmaX+oneN%*%meanX)
}

## multi-output ridge regression with lambda selection by PRESS
whitenridge<-function(TS,n,H,
                      verbose=FALSE,maha=FALSE, direct=FALSE, MIMO=FALSE,nLambdas=25,...){
  if (! (MIMO|direct))
    stop("Erro in multiridge: at least MIMO  or direct should be true")
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  sTS=scale(TS)
  m=NCOL(sTS)
  N=NROW(sTS)
  
  
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  WH=whiteMaha(YY)
  YY=WH$sX
  
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  ML<-mlin(XX,YY,H=H,maha=maha,nLambdas=nLambdas)
  beta.hat=ML$beta.hat 
  
  
  if (verbose)
    cat("lambda=",ML$lambda, "minMSE=",ML$minMSE,"\n")
  
  
  if (MIMO){
    ## MIMO prediction
    YhatM=c(1,Xts)%*%beta.hat
  }
  if (direct){
    ## direct prediction
    YhatD=numeric(NCOL(YY))
    for (j in 1:NCOL(YY))
      YhatD[j]<-c(1,Xts)%*%ML$beta.hatY[,j]
    
  }
  if (MIMO & ! direct)
    Yhat=YhatM
  if (!MIMO & direct)
    Yhat=YhatD
  
  if (MIMO & direct)
    Yhat=apply(rbind(YhatM,YhatD),2,mean)
  
  Yhat=colourMaha(array(Yhat,c(1,length(Yhat))),WH$mX,WH$Isigma)
  
  Yhat=array(Yhat,c(H,m))
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  return(list(Yhat=Yhat,MSE=ML$minuMSE))
}


## multi-output ridge regression with lambda selection by PRESS
multiridge<-function(TS,n,H,
                     verbose=FALSE,maha=FALSE, direct=FALSE, 
                     MIMO=FALSE,preq=FALSE,nLambdas=25,...){
  if (! (MIMO|direct))
    stop("Erro in multiridge: at least MIMO  or direct should be true")
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  sTS=scale(TS)
  m=NCOL(sTS)
  N=NROW(sTS)
  
  
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  if (!preq)
    ML<-mlin(XX,YY,H=H,maha=maha,nLambdas=nLambdas)
  else 
    ML<-multipreq(XX,YY,H=H)
  beta.hat=ML$beta.hat 
  
  
  if (verbose)
    cat("lambda=",ML$lambda, "minMSE=",ML$minMSE,"\n")
  
  
  if (MIMO){
    ## MIMO prediction
    YhatM=c(1,Xts)%*%beta.hat
  }
  if (direct){
    ## direct prediction
    YhatD=numeric(NCOL(YY))
    for (j in 1:NCOL(YY))
      YhatD[j]<-c(1,Xts)%*%ML$beta.hatY[,j]
    
  }
  if (MIMO & ! direct)
    Yhat=YhatM
  if (!MIMO & direct)
    Yhat=YhatD
  
  if (MIMO & direct)
    Yhat=apply(rbind(YhatM,YhatD),2,mean)
  
  Yhat=array(Yhat,c(H,m))
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  return(list(Yhat=Yhat,MSE=ML$minuMSE))
}


## multi-output ridge regression with lambda selection by PRESS
multiteridge<-function(TS,n,H,Hobj=1,
                       verbose=FALSE,minLambda=0.1,
                       maxLambda=1000,nLambdas=25,...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  sTS=scale(TS)
  m=NCOL(sTS)
  N=NROW(sTS)
  
  
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+1,1:m)
  
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  N<-NROW(XX) # number training data
  nn<-NCOL(XX) # number input variables
  m<-NCOL(YY)
  p<-nn+1
  XX<-cbind(array(1,c(N,1)),as.matrix(XX))
  XXX<-t(XX)%*%(XX)
  
  min.MSE.loo<-Inf
  bestlambdaY<-numeric(m)
  min.MSE.looY<-numeric(m)+Inf
  for (lambdah in seq(minLambda,maxLambda,length.out=nLambdas)){
    H1<-
      ginv(XXX+lambdah*diag(p))
    
    beta.hat<-H1%*%t(XX)%*%YY
    HH=XX%*%H1%*%t(XX)
    
    Y.hat<-HH%*%YY
    e<-YY-Y.hat
    e.loo<-e
    Y.loo=YY
    
    for (j in 1:NCOL(e)){
      e.loo[,j]<-e[,j]/pmax(1-diag(HH),0.01)
      w.na<-which(is.na(e.loo[,j]))
      if (length(w.na)>0)
        e.loo[w.na,j]=1
      Y.loo[,j]=Y.loo[,j]-e.loo[,j]
      
    }
    MSE.loo<-NULL
    for (i in seq(1,(NROW(e.loo)-H),by=1)){
      ERRITER<-array(0,c(10,NCOL(sTS)))
      for (h in 1:Hobj){
        N=NROW(ERRITER)
        delta<-0
        for (jj in 1:m)
          delta<-c(delta,ERRITER[seq(N-D,N-n+1-D,by=-1),jj])
        delta=array(delta,c(1,length(delta)))
        
        MSE.loo<-c(MSE.loo,(e.loo[i+h-1,]+delta%*%beta.hat)^2)
        ERRITER<-rbind(ERRITER,e.loo[i+h-1,]+delta%*%beta.hat)
      }
    }
    MSE.loo<-mean(MSE.loo)
    if (MSE.loo<min.MSE.loo){
      lambda<-lambdah
      min.MSE.loo<-MSE.loo
      
    }
  }
  H1<- ginv(XXX+lambda*diag(p))
  
  beta.hat<-H1%*%t(XX)%*%YY
  
  
  Yhat=NULL
  sTS2=sTS
  for (h in 1:H){
    
    N=NROW(sTS2)
    D=0
    q<-NULL
    for (j in 1:m)
      q<-c(q,sTS2[seq(N-D,N-n+1-D,by=-1),j])
    Xts=array(q,c(1,length(q)))
    
    Yh1=c(1,Xts)%*%beta.hat
    Yhat=rbind(Yhat,Yh1)
    sTS2<-rbind(sTS2,Yh1)
  }
  
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  return(list(Yhat=Yhat))
}

multiteridgeMC<-function(TS,n,H,
                         verbose=FALSE,minLambda=0.1,
                         maxLambda=1000,nLambdas=25,R=100,...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  YDIR=multiridge(TS,n,H, direct=TRUE)$Yhat
  sTS=scale(TS)
  m=NCOL(sTS)
  N=NROW(sTS)
  
  
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+1,1:m)
  
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  N<-NROW(XX) # number training data
  nn<-NCOL(XX) # number input variables
  m<-NCOL(YY)
  p<-nn+1
  XX<-cbind(array(1,c(N,1)),as.matrix(XX))
  XXX<-t(XX)%*%(XX)
  
  min.MSE.loo<-Inf
  bestlambdaY<-numeric(m)
  min.MSE.looY<-numeric(m)+Inf
  for (lambdah in seq(minLambda,maxLambda,length.out=nLambdas)){
    H1<-
      ginv(XXX+lambdah*diag(p))
    
    beta.hat<-H1%*%t(XX)%*%YY
    HH=XX%*%H1%*%t(XX)
    sde<-sqrt(mean(YY-HH%*%YY)^2)
    Emc<-NULL
    for (r in 1:R){
      Yhat=NULL
      sTS2=sTS
      for (h in 1:H){
        
        N=NROW(sTS2)
        D=0
        q<-NULL
        for (j in 1:m)
          q<-c(q,sTS2[seq(N-D,N-n+1-D,by=-1),j])
        Xts=array(q,c(1,length(q)))
        
        Yh1=c(1,Xts)%*%beta.hat
        Yhat=rbind(Yhat,Yh1)
        sTS2<-rbind(sTS2,Yh1+rnorm(length(Yh1),sd=sde))
      }
      for (ii in 1:NCOL(Yhat))
        Yhat[,ii]=Yhat[,ii]*attr(sTS,'scaled:scale')[ii]+attr(sTS,'scaled:center')[ii]
      
      Emc<-c(Emc,mean((YDIR-Yhat)^2))
    }
    
    
    if (mean(Emc)<min.MSE.loo){
      lambda<-lambdah
      min.MSE.loo<-mean(Emc)
      
    }
  }
  H1<- ginv(XXX+lambda*diag(p))
  
  beta.hat<-H1%*%t(XX)%*%YY
  
  
  Yhat=NULL
  sTS2=sTS
  for (h in 1:H){
    
    N=NROW(sTS2)
    D=0
    q<-NULL
    for (j in 1:m)
      q<-c(q,sTS2[seq(N-D,N-n+1-D,by=-1),j])
    Xts=array(q,c(1,length(q)))
    
    Yh1=c(1,Xts)%*%beta.hat
    Yhat=rbind(Yhat,Yh1)
    sTS2<-rbind(sTS2,Yh1)
  }
  
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  return(list(Yhat=Yhat))
}


ensridge<-function(TS,n,H,
                   verbose=FALSE,minLambda=0.1,
                   maxLambda=1000,nLambdas=25,...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  sTS=scale(TS)
  m=NCOL(sTS)
  N=NROW(sTS)
  
  
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  XX<-cbind(array(1,c(NROW(XX),1)),as.matrix(XX))
  XXX<-t(XX)%*%(XX)
  W<-NULL
  Yhat<-NULL
  for (lambdah in seq(minLambda,maxLambda,length.out=nLambdas)){
    H1<- ginv(XXX+lambdah*diag(NCOL(XX)))
    beta.hat<-H1%*%t(XX)%*%YY
    HH=XX%*%H1%*%t(XX)
    
    Y.hat<-HH%*%YY
    e<-YY-Y.hat
    e.loo<-e
    Y.loo=YY
    for (j in 1:NCOL(e)){
      e.loo[,j]<-e[,j]/pmax(1-diag(HH),0.01)
      w.na<-which(is.na(e.loo[,j]))
      if (length(w.na)>0)
        e.loo[w.na,j]=1
      Y.loo[,j]=Y.loo[,j]-e.loo[,j]
      
    }
    Yhatall<-c(1,Xts)%*%beta.hat
    seqB= 2:round(1.2*NCOL(e.loo))
    for (b in c(1,seqB)){
      colsample<-sample(1:NCOL(e.loo),round(NCOL(e.loo)/10))
      if (b==1) ## MIMO 
        colsample<-1:NCOL(e.loo)
      if (b>1 & b< (NCOL(e.loo)+1)) ## DIRECT
        colsample<-c(b-1,b-1)
      w<-numeric(NCOL(e))+NA
      yhat<-numeric(NCOL(e))+NA
      
      MSE.loo<-mean(e.loo[,colsample]^2)
      w[colsample]=1/MSE.loo
      W<-rbind(W,w)
      yhat[colsample]<-Yhatall[colsample]*w[colsample]
      Yhat<-rbind(Yhat,yhat)
    } ## for b
    ## ITERATED
    beta.hatITER<-beta.hat[,seq(1,NCOL(YY),by=H)]
    e.looITER<-e.loo[,seq(1,NCOL(YY),by=H)]
    MSE.looITER<-list()
    
    MSE.looITER[[H+1]]<-0
    
    for (i in seq(max(1,NROW(e.looITER)-6*H),(NROW(e.looITER)-H),by=2)){
      ERRITER<-array(0,c(10,NCOL(sTS)))
      for (h in 1:H){
        N=NROW(ERRITER)
        delta<-0
        for (jj in 1:m)
          delta<-c(delta,ERRITER[seq(N-D,N-n+1-D,by=-1),jj])
        delta=array(delta,c(1,length(delta)))
        
        MSE.looITER[[h]]<-c(MSE.looITER[[h]],(e.looITER[i+h,]+delta%*%beta.hatITER)^2)
        ERRITER<-rbind(ERRITER,e.looITER[i+h,]+delta%*%beta.hatITER)
      } ## for h
    }
    YhatITER=numeric(NCOL(Yhat))
    sTS2=sTS
    w=numeric(NCOL(Yhat))
    for (h in 1:H){
      N=NROW(sTS2)
      D=0
      qITER<-NULL
      for (j in 1:m)
        qITER<-c(qITER,sTS2[seq(N-D,N-n+1-D,by=-1),j])
      XtsITER=array(qITER,c(1,length(qITER)))
      
      Yh1=c(1,XtsITER)%*%beta.hatITER
      w[seq(h,NCOL(YY),by=H)]<-1/mean(MSE.looITER[[h]])
      YhatITER[seq(h,NCOL(YY),by=H)]=Yh1*1/mean(MSE.looITER[[h]])
      
      sTS2<-rbind(sTS2,Yh1)
    }
    
    W<-rbind(W,w)
    Yhat<-rbind(Yhat,YhatITER)
    
    
  }## for lambda
  
  
  
  Yhat=apply(Yhat,2,sum,na.rm=TRUE)/apply(W,2,sum,na.rm=TRUE)
  Yhat=array(Yhat,c(H,m))
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  return(list(Yhat=Yhat))
}


## multi-output learner
multiml<-function(TS,n,H,learner,
                  verbose=FALSE,...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  
  sTS=scale(TS)
  m=NCOL(sTS)
  N=NROW(sTS)
  
  
  M=MakeEmbedded(sTS,numeric(m)+n,numeric(m),numeric(m)+H,1:m)
  
  I=which(!is.na(apply(M$out,1,sum)))
  XX=M$inp[I,]
  YY=M$out[I,]
  
  q<-NULL
  D=0
  for (j in 1:m)
    q<-c(q,sTS[seq(N-D,N-n+1-D,by=-1),j])
  Xts=array(q,c(1,length(q)))
  
  Yhat<-pred(learner,XX,YY,Xts,classi=FALSE)
  Yhat<-array(Yhat,c(H,m))
  
  
  for (i in 1:NCOL(Yhat))
    Yhat[,i]=Yhat[,i]*attr(sTS,'scaled:scale')[i]+attr(sTS,'scaled:center')[i]
  return(Yhat=Yhat)
}

multifs3<-function(TS,n,H,mod,...){
  args<-list(...)
  if (length(args)>0)
    for(i in 1:length(args)) {
      assign(x = names(args)[i], value = args[[i]])
    }
  m=NCOL(TS)
  N=NROW(TS)
  Yhat<-array(NA,c(H,m))
  for (i in 1:m){
    Ii=setdiff(1:m,i)
    fs<-Ii[mrmr(TS[,Ii],TS[,i],min(m-1,5))]
    ## subset of series which is informative about TS[,i]
    Yhat[,i]=multifs(TS[,c(i,fs)],n,H,mod=mod,w=1)
    
  }
  
  
  return(Yhat)
}


VARpred2<-function (x,model, h = 1, orig = 0, Out.level = F,verbose=FALSE) {
  
  Phi = model$Phi
  sig = model$Sigma
  Ph0 = model$Ph0
  p = model$order
  cnst = model$cnst
  np = dim(Phi)[2]
  k = dim(x)[2]
  nT = dim(x)[1]
  k = dim(x)[2]
  if (orig <= 0) 
    orig = nT
  if (orig > nT) 
    orig = nT
  psi = VARpsi(Phi, h)$psi
  beta = t(Phi)
  if (length(Ph0) < 1) 
    Ph0 = rep(0, k)
  if (p > orig) {
    cat("Too few data points to produce forecasts", "\n")
  }
  pred = NULL
  se = NULL
  MSE = NULL
  mse = NULL
  px = as.matrix(x[1:orig, ])
  Past = px[orig, ]
  if (p > 1) {
    for (j in 1:(p - 1)) {
      Past = c(Past, px[(orig - j), ])
    }
  }
  if (verbose)
    cat("orig ", orig, "\n")
  ne = orig - p
  xmtx = NULL
  P = NULL
  if (cnst) 
    xmtx = rep(1, ne)
  xmtx = cbind(xmtx, x[p:(orig - 1), ])
  ist = p + 1
  if (p > 1) {
    for (j in 2:p) {
      xmtx = cbind(xmtx, x[(ist - j):(orig - j), ])
    }
  }
  xmtx = as.matrix(xmtx)
  G = t(xmtx) %*% xmtx/ne
  Ginv = solve(G)
  P = Phi
  vv = Ph0
  if (p > 1) {
    II = diag(rep(1, k * (p - 1)))
    II = cbind(II, matrix(0, (p - 1) * k, k))
    P = rbind(P, II)
    vv = c(vv, rep(0, (p - 1) * k))
  }
  if (cnst) {
    c1 = c(1, rep(0, np))
    P = cbind(vv, P)
    P = rbind(c1, P)
  }
  Sig = sig
  n1 = dim(P)[2]
  MSE = (n1/orig) * sig
  for (j in 1:h) {
    tmp = Ph0 + matrix(Past, 1, np) %*% beta
    px = rbind(px, tmp)
    if (np > k) {
      Past = c(tmp, Past[1:(np - k)])
    }
    else {
      Past = tmp
    }
    if (j > 1) {
      idx = (j - 1) * k
      wk = psi[, (idx + 1):(idx + k)]
      Sig = Sig + wk %*% sig %*% t(wk)
    }
    if (j > 1) {
      for (ii in 0:(j - 1)) {
        psii = diag(rep(1, k))
        if (ii > 0) {
          idx = ii * k
          psii = psi[, (idx + 1):(idx + k)]
        }
        P1 = P^(j - 1 - ii) %*% Ginv
        for (jj in 0:(j - 1)) {
          psij = diag(rep(1, k))
          if (jj > 0) {
            jdx = jj * k
            psij = psi[, (jdx + 1):(jdx + k)]
          }
          P2 = P^(j - 1 - jj) %*% G
          k1 = sum(diag(P1 %*% P2))
          MSE = (k1/orig) * psii %*% sig %*% t(psij)
        }
      }
    }
    se = rbind(se, sqrt(diag(Sig)))
    if (Out.level) {
      cat("Covariance matrix of forecast errors at horizon: ", 
          j, "\n")
      if (verbose){
        print(Sig)
        cat("Omega matrix at horizon: ", j, "\n")
        print(MSE)
      }
    }
    MSE = MSE + Sig
    mse = rbind(mse, sqrt(diag(MSE)))
  }
  if (verbose){
    cat("Forecasts at origin: ", orig, "\n")
    print(px[(orig + 1):(orig + h), ], digits = 4)
    cat("Standard Errors of predictions: ", "\n")
    print(se[1:h, ], digits = 4)
  }
  pred = px[(orig + 1):(orig + h), ]
  if (verbose){
    cat("Root mean square errors of predictions: ", "\n")
    print(mse[1:h, ], digits = 4)
  }
  if (orig < nT) {
    if (verbose){
      cat("Observations, predicted values,     errors, and MSE", 
          "\n")
    }
    tmp = NULL
    jend = min(nT, (orig + h))
    for (t in (orig + 1):jend) {
      case = c(t, x[t, ], px[t, ], x[t, ] - px[t, ])
      tmp = rbind(tmp, case)
    }
    colnames(tmp) <- c("time", rep("obs", k), rep("fcst", 
                                                  k), rep("err", k))
    idx = c(1)
    for (j in 1:k) {
      idx = c(idx, c(0, 1, 2) * k + j + 1)
    }
    tmp = tmp[, idx]
    if (verbose)
      print(round(tmp, 4))
  }
  VARpred <- list(pred = pred, se.err = se, mse = mse)
}

detectSeason<-function(TS,maxs=20,Ls=100,pmin=0.1,forced=FALSE, debug=FALSE){
  ## forced force the detection of seasonality
  ## Ls length total output series
  if (length(TS)<20 || sd(TS)<0.01)
    return(list(best=1,spattern=numeric(Ls),strend=numeric(Ls)))
  if (any(is.infinite(TS)))
    return(list(best=1,spattern=numeric(Ls),strend=numeric(Ls)))
  if (sd(TS,na.rm=TRUE)<0.01)
    return(list(best=1,spattern=numeric(Ls),strend=numeric(Ls)))
  
  N=length(TS)
  if (!forced){
    maxs=min(maxs,round(N/5))
  }else {
    pmin=0.5
  }
  trndmod=lm(TS ~ seq(TS))
  trnd=numeric(N)
  summmod=summary(trndmod)
  # check 
  if (pf(summmod$fstatistic[1],summmod$fstatistic[2],summmod$fstatistic[3],lower.tail=FALSE)<pmin)
    trnd=trndmod$fit
  
  VS=numeric(maxs)-Inf
  
  S<-TS-trnd  ## detrended series
  
  for (s in 4:maxs){
    PV=NULL
    V=NULL
    m_S = t(matrix(data = S[1:(floor(N/s)*s)], nrow = s))
    sdlS=confsd(S,alpha=pmin)$low
    ## lower bound standard deviation of time series
    
    VS[s]=(sdlS-mean(unlist(lapply(apply(m_S,2,confsd,pmin),'[[',"upp"))))/sdlS
    ## percentual reduction of lower bound of stdev(TS) with respect to upperbound of conditional variances:
    ## measure of entropy reduction 
    
  }# add
  
  mVS=max(VS)
  if (debug)
    browser()
  I=1:Ls
  if (sd(trnd)<0.01)
    trnd2=numeric(Ls)
  else
    trnd2=pred("lin",1:length(trnd),trnd,1:Ls,classi=FALSE,lambda=1e-3)
  if (mVS>0 | forced) { 
    
    bests=which.max(VS)  ## lowest conditional variance 
    
    m_S = t(matrix(data = S[1:(floor(N/bests)*bests)], nrow = bests))
    spattern=apply(m_S,2,mean)
    spattern=rep(spattern,length.out=Ls)
  } else {
    bests=NA
    spattern=numeric(Ls)
  }
  return(list(best=bests,spattern=spattern,strend=trnd2))
}#


confsd<-function(x,alpha=0.01){
  N=length(x)
  chir=qchisq(alpha/2,N-1,lower.tail=TRUE)
  chil=qchisq(alpha/2,N-1,lower.tail=FALSE)
  return(list(low=sqrt((N-1)*var(x)/chil), upp=sqrt((N-1)*var(x)/chir)))
  
}

