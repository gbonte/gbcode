#' ranking
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description Ranking filter based on mutual information
#' @details Ranking filter based on mutaul information
#' @title rankrho
#' @name rankrho
#' @export
#'
#' @param  X: input dataset
#' @param Y: output dataset
#' @param nmax: number of top returned features
#' @return Indices of \code{nmax} top ranked features
#'
#' @examples
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1,seed=0)
#' X<-R$X
#' Y<-R$Y
#' real.features<-R$feat
#' ranked.features<-rankrho(X,Y,nmax=3)



rankrho<-function(X,Y,nmax=5,regr=FALSE){

  m<-NCOL(Y)
  ## number of outputs

  X<-scale(X)

  Iy<-cor2I2(corXY(X,Y))

  if (m>1)
    Iy<-apply(Iy,1,mean)

  return(sort(c(Iy), decreasing=T, index.return=T)$ix[1:nmax])


}



#' mrmr
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description mRMR filter based on mutual information
#' @details mRMR (minimum redundancy maximum relevance) filter based on mutual information
#' @title mrmr
#' @name mrmr
#' @export
#'
#' @param  X: input dataset
#' @param Y: output dataset
#' @param nmax: number of top returned features
#' @param back: if TRUE, backward reordering based on linear regression
#' @return Indices of \code{nmax} top ranked features
#'
#' @examples
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1,seed=0)
#' X<-R$X
#' Y<-R$Y
#' real.features<-R$feat
#' ranked.features<-mrmr(X,Y,nmax=3)
mrmr<-function(X,Y,nmax=5,back=FALSE){

  n<-NCOL(X)
  N<-NROW(X)
  m<-NCOL(Y)

  if (is.factor(X[,1]) && is.factor(Y)){
    require(infotheo)
    Iy<-numeric(n)
    Ix<-array(0,c(n,n))
    for (i in 1:n){
      for (j in 1:n){
        Ix[i,j]<-mutinformation(X[,i],X[,j])
      }
      Iy[i]<-mutinformation(X[,i],Y)
    }
  }else {
    X<-scale(X)
    Iy<-cor2I2(corXY(X,Y))
    CCx<-cor(X,use="pairwise.complete.obs")
    Ix<-cor2I2(CCx)
  }

  subs<-which.max(Iy)
  for (j in length(subs):min(n-1,nmax)){
    mrmr<-numeric(n)-Inf
    if (length(subs)<(n-1)){
      if (length(subs)>1){
        mrmr[-subs]<- Iy[-subs]+apply(-Ix[subs,-subs],2,mean)
      } else {
        mrmr[-subs]<- Iy[-subs]+(-Ix[subs,-subs])
      }
    } else {
      mrmr[-subs]<-Inf
    }

    s<-which.max(mrmr)
    subs<-c(subs,s)
  }

  if (back){  ## backward reordering based on linear regression
    nsubs<-NULL
    while (length(subs)>1){
      pd<-numeric(length(subs))
      for (ii in 1:length(subs))
        pd[ii]<-regrlin(X[,setdiff(subs,subs[ii])],Y)$MSE.emp
      nsubs<-c(subs[which.min(pd)],nsubs)
      subs<-setdiff(subs,subs[which.min(pd)])
    }
    subs<-c(subs,nsubs)
  }
  subs[1:nmax]
}


#' cmim
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description cmim filter based on mutual information
#' @details cmim (Fleuret) filter based on mutual information
#' @title cmim
#' @name cmim
#' @export
#'
#' @param  X: input dataset
#' @param Y: output dataset
#' @param nmax: number of top returned features
#' @param back: if TRUE, backward reordering based on linear regression
#' @return Indices of \code{nmax} top ranked features
#'
#' @examples
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1,seed=0)
#' X<-R$X
#' Y<-R$Y
#' real.features<-R$feat
#' ranked.features<-cmim(X,Y,nmax=3)
cmim<-function(X,Y,nmax=5,back=TRUE){

  X<-scale(X)
  Y<-scale(Y)
  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  N<-NROW(X)
  CY<-cor(X,Y[,1])
  subset<-1:n
  Iy<-array(NA,c(n,m))
  Ixx<-array(NA,c(n,n))

  for (i in 1:(n))
    for (j in setdiff(1:n,i))
      Ixx[i,j]<-cor2I2(pcor1(X[,i],Y,X[,j]))

  for (i in 1:m){
    Iy[,i]<-cor2I2(cor(X,Y[,i],use="pairwise.complete.obs"))

  }

  mrmr<-array(0,c(n,m))
  subs<-which.max(Iy)
  for (j in length(subs):min(n-2,nmax)){
    mrmr<-numeric(n)-Inf
    if (length(subs)<(n-1) & length(subs)>1){
      mrmr[-subs]<- apply(Ixx[-subs,subs],1,min)
    } else {
      mrmr[-subs]<- Ixx[-subs,subs]

    }
    s<-which.max(mrmr)
    subs<-c(subs,s)
  }


  if (back){
    nsubs<-NULL
    while (length(subs)>1){
      pd<-numeric(length(subs))

      for (ii in 1:length(subs))
        pd[ii]<-regrlin(X[,setdiff(subs,subs[ii])],Y)$MSE.emp
      nsubs<-c(subs[which.min(pd)],nsubs)
      subs<-setdiff(subs,subs[which.min(pd)])


    }
    subs<-c(subs,nsubs)
  }


  subset[subs[1:nmax]]

}




#' rankregr
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description Feature ranking based on linear least squares regression coefficients
#' @details Feature ranking based on linear least squares regression coefficients
#' @title rankregr
#' @name rankregr
#' @export
#'
#' @param  X: input dataset
#' @param Y: output dataset
#' @param nmax: number of top returned features
#' @return Indices of \code{nmax} top ranked features
#'
#' @examples
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1,seed=0)
#' X<-R$X
#' Y<-R$Y
#' real.features<-R$feat
#' ranked.features<-rankregr(X,Y,nmax=3)
rankregr<-function(X,Y,lambda=0.01,nmax=5){
  n<-NCOL(X)
  N<-NROW(X)
  m<-NCOL(Y)
  R<-regrlin(X,Y,lambda=lambda)
  return(sort(abs(R$beta.hat[2:(n+1)]),decr=T,ind=T)$ix[1:nmax])
}


#' linearFsel
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description Wrapper feature selection based on forward selection and linear regression
#' @details Wrapper feature selection based on forward selection and linear regression
#' @title linearFsel
#' @name linearFsel
#' @export
#'
#' @param  X: input dataset
#' @param Y: output dataset
#' @param nmax: number of top returned features
#' @param nmax2: number of forward selection steps
#' @param back: if TRUE, backward reordering based on linear regression
#' @param loo: if TRUE, assessment based on leave-one-out MSE
#' @return Indices of \code{nmax} top ranked features
#'
#' @examples
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1,seed=0)
#' X<-R$X
#' Y<-R$Y
#' real.features<-R$feat
#' ranked.features<-linearFsel(X,Y,nmax=3)
linearFsel<-function(X,Y,nmax=5,nmax2=NCOL(X),loo=FALSE,back=FALSE){
  subset<-which(apply(X,2,sd)>0)
  ## it removes constant features

  X<-X[,subset]

  X<-scale(X)
  Y<-as.numeric(Y)
  Y<-scale(Y)
  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  N<-NROW(X)

  NMAX<-min(nmax,nmax2)
  if (back)
    NMAX<-min(n,2*nmax)
  CY<-corXY(X,Y)

  subs<-which.max(abs(CY))
  for (j in length(subs):NMAX){
    subs<-subs[1:j]
    pd<-numeric(n)+Inf
    for (i in setdiff(1:n,subs)){
      if (loo)
        pd[i]<-regrlin(X[,c(subs,i)],Y)$MSE.loo
      else
        pd[i]<-regrlin(X[,c(subs,i)],Y)$MSE.emp
    }
    s<-which.min(pd)
    subs<-c(subs,sort(pd,decr=FALSE,ind=TRUE)$ix[1:(n-length(subs))])
  }

  if (back){
    nsubs<-NULL
    while (length(subs)>1){
      pd<-numeric(length(subs))
      for (ii in 1:length(subs))
        if (loo)
          pd[ii]<-regrlin(X[,setdiff(subs,subs[ii])],Y)$MSE.loo
        else
          pd[ii]<-regrlin(X[,setdiff(subs,subs[ii])],Y)$MSE.emp
        nsubs<-c(subs[which.min(pd)],nsubs)
        subs<-setdiff(subs,subs[which.min(pd)])
    }
    subs<-c(subs,nsubs)
  }
  subset[subs[1:nmax]]

}




errfunction<-function(X,Y,algo,cv=10, classi=TRUE,...){
  ## returns empirical error if cv<=1, cross-validation error if cv>1

  N<-NROW(X)
  n<-NCOL(X)
  if (classi){ ## classification

    if (cv<=1){
      p<-pred(algo,X,Y,X,...)$pred
      erri<-length(which(p!=Y))/length(Y)
      return(erri)
    }

    size.fold<-round(N/cv)
    err.cv<-NULL
    for (i in 1:cv){
      i.ts<-((i-1)*size.fold+1):min(N,i*size.fold)
      i.tr<-setdiff(1:N,i.ts)

      if (n==1)
        p<-pred(algo,X[i.tr],Y[i.tr],X[i.ts],...)$pred
      else
        p<-pred(algo,X[i.tr,],Y[i.tr],X[i.ts,],...)$pred
      err.cv<-c(err.cv,length(which(p!=Y[i.ts]))/length(i.ts))
    }
    return(mean(err.cv))

  } else {  ## regression

    if (cv==1){
      p<-pred(algo,X,Y,X,class=FALSE,...)
      erri<-mean((p-Y)^2)
      return(erri)
    }

    size.fold<-round(N/cv)
    err.cv<-NULL
    for (i in 1:cv){
      i.ts<-((i-1)*size.fold+1):min(N,i*size.fold)
      i.tr<-setdiff(1:N,i.ts)
      if (n==1)
        p<-pred(algo,X[i.tr],Y[i.tr],X[i.ts],class=FALSE,...)
      else
        p<-pred(algo,X[i.tr,],Y[i.tr],X[i.ts,],class=FALSE,...)
      err.cv<-c(err.cv,(p-Y[i.ts])^2)
    }
    return(mean(err.cv)/var(Y))
  }

}


eval.acc<-function(X,Y,algo=c("svm.lin"),cv=1,classi=TRUE,...){
  ## allows blocking feature selections
  ## 17/11/2011

  err<-NULL
  for (i in 1:length(algo)){
    err<-c(err,errfunction(X,Y,algo=algo[i],cv=cv,classi=classi,...))
  }

  mean(err)
}

#' forwardSel
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description Wrapper feature selection based on forward selection and a generic predictor
#' @details Wrapper feature selection based on forward selection and a generic predictor
#' @title forwardSel
#' @name forwardSel
#' @export
#'
#' @param  X: input dataset
#' @param Y: output dataset
#' @param algo: see the options of \link{pred}
#' @param nmax: number of top returned features
#' @param nmax2: number of forward selection steps
#' @param classi: if TRUE, classification problem else regression
#' @param back: if TRUE, backward reordering based on linear regression
#' @param cv: number of cross-validation folds (if \code{cv=1} no cross-validation)
#' @param verbose: if TRUE it prints out the MSE estimations
#' @return Indices of \code{nmax} top ranked features
#'
#' @examples
#' ## regression example
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1,seed=0)
#' X<-R$X
#' Y<-R$Y
#' real.features<-R$feat
#' ranked.features<-forwardSel(X,Y,nmax=3)
#'
#' ## classification example
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1,seed=1)
#' X<-R$X
#' Y<-factor(round(R$Y))
#' real.features<-R$feat
#' ranked.features<-forwardSel(X,Y,nmax=3,classi=TRUE,cv=3)
#'
forwardSel<-function(X,Y,algo="rf",nmax=5,nmax2=nmax,cv=1,classi=FALSE,verbose=FALSE,...){

  n<-NCOL(X)
  selected<-NULL
  for ( i in 1:nmax2){
    accuracy<-numeric(n)+NA
    if (i>1)
      accuracy[selected]<-Inf
    for (j in setdiff(1:n,selected))
      accuracy[j]<-eval.acc(X[,c(selected,j)],Y,algo,cv=cv,classi=classi,...)

    s<-sort(accuracy,index.return=TRUE)$ix

    selected<-c(selected,which.min(accuracy))
    if (verbose){
      print(accuracy)
      print(colnames(X)[selected])
    }
  }
  if (length(selected)<nmax)
    selected<-c(selected, s[2:(1+nmax-length(selected))])
  selected
}




gsloo<-function(X,
                Y,
                max.var=ncol(X),
                trace=FALSE,automatic=F,class=F,shave=F,est.par=F){
  # Gram-Schmidt

  if (class & (!is.factor(Y)))
    stop("Y should be a factor")

  if (class & (length(levels(Y))!=2))
    stop("Y should be a factor with 2 levels")

  if (class){
    L<-levels(Y)
    ind<-which(Y==L[1])
    Y<-as.numeric(Y)
    Y[ind]<-1
    Y[setdiff(1:length(Y),ind)]<--1
  }

  X<-scale(X)
  XX<-X
  selected<-NULL
  corr.var<-NULL
  n<-ncol(XX)
  N<-NROW(XX)
  muX<-apply(XX,2,mean)
  for (j in 1:n)
    XX[,j]<-(XX[,j]-muX[j])
  if (!class){
    muY<-mean(Y)
    Y<-(Y-muY)
    Y<-scale(Y)
  }

  lam<-0.02
  A<-diag(max.var)
  theta<-numeric(max.var)
  pval<-numeric(max.var)+1
  ind<-1:n
  n.Y<-normv(Y,2)
  e<-Y
  eta<-numeric(N)+1
  P<-NULL

  J<-NULL
  NMSE<-NULL
  max.cc<-numeric(NCOL(XX))
  for (k in 1:min(max.var,n)){
    X.Y<-t(XX)%*%Y  ## XX[N,n] Y [N,1] -> X.Y [n,1]
    n.M<-apply(XX,2,normv,2)
    cc<-(X.Y)^2/(n.M^2*(n.Y^2))
    if (k==1)
      max.cc<-pmax(cc,max.cc,na.rm=T)
    cc[is.na(cc)]<--Inf
    cc[corr.var]<--Inf


    s<-sort(cc,decreasing=TRUE,index.return=TRUE)


    x.sel<-XX[,s$ix[1]]
    g<-x.sel
    if (k==1){
      P<-cbind(P,XX[,s$ix[1]])
      p<-XX[,s$ix[1]]
      theta[1]<-(t(p)%*%Y)/(as.numeric(p%*%p))
      pval[1]<-cor.test(p,Y)$p.value
    } else {
      g2<-numeric(NROW(XX))
      for (j in 1:NCOL(P)){
        A[j,k]<-t(P[,j])%*%g/(as.numeric(P[,j]%*%P[,j]))
        g2<-g2+A[j,k]*P[,j]
      }
      p<-g-g2
      P<-cbind(P,p)
      theta[k]<-(t(p)%*%Y)/(as.numeric(p%*%p))
      pval[k]<-cor.test(p,Y)$p.value
    }
    e<-e-p*theta[k]
    eta<-eta-(p^2)/(as.numeric(p%*%p)+lam)

    if (!class){
      J.regr<-sqrt(mean((e/eta)^2))
      NMSE.regr<-J.regr/(var(Y))
    } else {

      ss<-1-(1-Y*(Y-e))/eta   #Y.hat<-Y-e  => e=Y-Y.hat
      J.class<-mean(ss<=0)
    }
    if (length(J)>1){
      if (!class){
        if (is.na(J.regr))
          browser()
        if (automatic & (J.regr>J[length(J)] ) ){
          A<-A[1:(k-1),1:(k-1)]
          theta<-theta[1:k-1]
          break
        }
      } else {
        if (automatic &  (J.class>J[length(J)]) | J.class<0) {
          A<-A[1:(k-1),1:(k-1)]
          theta<-theta[1:k-1]
          break

        }

      }
    }

    if (!class){
      J<-cbind(J,J.regr)
      NMSE<-c(NMSE,NMSE.regr)
    } else {
      J<-cbind(J,J.class)
    }

    selected<-c(selected,s$ix[1])
    corr.var<-c(corr.var,s$ix[1])

    XX<- XX-x.sel%*%(x.sel%*%XX)/as.numeric(x.sel%*%x.sel)
    Y<- Y-x.sel%*%(x.sel%*%Y)/as.numeric(x.sel%*%x.sel)


    if (shave){
      sd.XX<-apply(XX,2,sd)
      rel.sd<-sd.XX
      which.compress<-setdiff(which(rel.sd <quantile(rel.sd,prob=0.05) | rel.sd<0.25),corr.var)


      if (length(which.compress)>0){
        print(min(sd.XX[which.compress]))

        Z<-pca(X[,c(corr.var,which.compress)],n.comp=length(selected))

        if (T){
          XX<-X
          for (s in 1:length(selected)){
            x.sel<-Z$data[,s]
            cor.sel<-(x.sel %*%XX)/as.numeric(x.sel%*%x.sel)
            XX<- XX-x.sel%*%cor.sel

          }
        }
        ##  XX[,selected]<-Z$data[,1:length(selected)]  useless
        corr.var<-c(corr.var,which.compress)


      }
    }




    if (trace)
    {
      print(selected)
      print(NMSE)


    }
    rm(X.Y,x.sel,cc,s,n.M)
    gc()
  }

  if (est.par){
    par<-solve(A,theta,LINPACK=T)
    par<-c(muY-muX[selected]%*%par,par)
  }
  list(sel=corr.var,par=par,Jloo=J,NMSE=NMSE,max.cor=max.cc,pval=pval)

}







strimmer.rank<-function(X,Y,nmax){

  n<-NCOL(X)
  XX<-cbind(X,Y)
  G<-ggm.estimate.pcor(XX,verbose=FALSE)
  pc<-G[n,-n]

  sort(abs(pc),decr=T,ind=T)$ix[1:nmax]




}



mimr<-function(X,Y,nmax=5,first=NULL,
               init=FALSE,lambda=0.5,
               fast.inter=0,back=FALSE,
               spouse.removal=TRUE,
               caus=1){
  ## mimr filter
  # if caus =1 it searches for causes otherwise if caus=-1 it searches for effects


  NMAX<-nmax
  if (back)
    NMAX<-2*nmax
  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  orign<-n
  N<-NROW(X)
  H<-apply(X,2,var)
  HY<-var(Y)
  CY<-corXY(X,Y)
  Iy<-cor2I2(CY)
  subset<-1:n
  pv.rho<-ppears(c(CY),N+numeric(n))
  if (spouse.removal){
    pv<-ppears(c(CY),N+numeric(n))
    s<-sort(pv,decreasing=TRUE,index.return=TRUE)
    hw<-min(n-nmax,length(which(s$x>0.05)))
    spouse<-s$ix[1:hw]
    subset<-setdiff(1:n,s$ix[1:hw])
    X<-X[,subset]
    Iy<-Iy[subset]
    n<-NCOL(X)
  }


  CCx<-cor(X)
  Ix<-cor2I2(CCx)
  Ixx<-Icond(X,z=Y,lambda=0.02)
  Inter<-array(NA,c(n,n))

  if (init){
    max.kj<--Inf
    for (kk in 1:(n-1)){
      for (jj in (kk+1):n){
        Inter[kk,jj]<- (1-lambda)*(Iy[kk]+Iy[jj])+caus*lambda*(Ixx[kk,jj]-Ix[kk,jj])
        Inter[jj,kk]<-Inter[kk,jj]
        if (Inter[kk,jj]>max.kj){
          max.kj<-Inter[kk,jj]
          subs<-c(kk,jj)
        }
      }
    }
  } else {
    subs<-which.max(Iy)
  }

  if (nmax>length(subs)){
    last.subs<-0
    for (j in length(subs):min(n-1,NMAX-1)){
      mrmr<-numeric(n)-Inf
      if (length(subs)<(n-1)){
        if (length(subs)>1){
          mrmr[-subs]<- (1-lambda)*Iy[-subs]+caus*lambda*apply(-Ix[subs,-subs]+Ixx[subs,-subs],2,mean)
        } else {
          mrmr[-subs]<- (1-lambda)*Iy[-subs]+caus*lambda*(-Ix[subs,-subs]+Ixx[subs,-subs])
        }
      } else {
        mrmr[-subs]<-Inf
      }
      s<-which.max(mrmr)
      subs<-c(subs,s)
    }


    if (back){
      nsubs<-NULL

      while (length(subs)>1){
        pd<-numeric(length(subs))
        for (ii in 1:length(subs))
          pd[ii]<-regrlin(X[,setdiff(subs,subs[ii])],Y)$MSE.loo
        nsubs<-c(subs[which.min(pd)],nsubs)
        subs<-setdiff(subs,subs[which.min(pd)])
      }
      subs<-c(subs,nsubs)
    } # if back

  }

  ra<-subset[subs]

  if (nmax>length(ra))
    ra<-c(ra,setdiff(1:orign,ra))

  ra

}


mimrback<-function(X,Y,lambda=0.5,lambda2=lambda,lag=1,nmax=10,back=FALSE,init=FALSE,mn=T){
  ## maximizes conditional information on all the remaining features
  ## 25/10/11
  require(corpcor)
  n<-NCOL(X)
  Iy<-cor2I2(corXY(X,Y))
  Ixx<-Icond(X,z=Y)
  Ix<-cor2I2(cor(X))


  subs<-which.min(Iy)

  for (i in 1:(n-2)){
    pd<-numeric(n)+Inf

    if (length(subs)>1)
      pd[-subs]<- (1-lambda)*(Iy[-subs])+lambda*(apply(-Ix[subs,-subs]+Ixx[subs,-subs],2,mean))
    else
      pd[-subs]<- (1-lambda)*(Iy[-subs])+lambda*((-Ix[subs,-subs]+Ixx[subs,-subs]))
    subs<-c(which.min(pd),subs)


  }
  subs<-c(setdiff(1:n,subs),subs)




  subs[1:nmax]
}

mCRMR<-function(X,Y,lambda=0.5,nmax=10,maxpert=FALSE){
  ## ranking according to a cost function maximizing relevance and minimizing conditional relevance of others


  n<-NCOL(X)
  Ixx<-Icond(X,z=Y,lambda=0.5)
  Ix<-cor2I2(cor(X)) ##array(0,c(n,n))
  Iy<-cor2I2(corXY(X,Y))
  H<-var2H(apply(X,2,var))


  cut<-numeric(n)
  if (!maxpert)
    for (i in 1:n){
      cut[i]<-(1-lambda)*(Iy[i]-H[i])-lambda*mean(Iy[-i]-Ix[i,-i]+Ixx[i,-i])
      ####            max I(y;x_i)                   min  mean_j {I(y;xj| xi)}

    } else {
      for (i in 1:n){  ## max perturbation
        cut[i]<-(1-lambda)*(Iy[i]-H[i])-lambda*mean(-Ix[i,-i]+Ixx[i,-i])
        ####                          max  mean_j {I(y,x_j)- I(y;xj| xi)}

      }
    }

  subs<-sort(cut,decr=T,ind=T)$ix[1:nmax]
  # to be maximized



}




bmimr<-function(X,Y,nmax=5,first=NULL,init=TRUE,lambda=0.95,back=TRUE){

  X<-scale(X)
  Y<-scale(Y)
  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  N<-NROW(X)

  CY<-cor(X,Y[,1])

  subset<-1:n


  CCx<-cor(X)
  Iy<-array(NA,c(n,m))
  Iy2<-array(NA,c(n,m))
  Ix<-cor2I2(CCx)
  Ixx<-array(NA,c(n,n))



  for (i in 1:(n-1))
    for (j in (i+1):n){
      Ixx[i,j]<-cor2I2(pcor1(X[,i],X[,j],Y))
      Ixx[j,i]<-Ixx[i,j]
    }


  for (i in 1:m){
    Iy[,i]<-cor2I2(cor(X,Y[,i],use="pairwise.complete.obs"))

  }

  nsubs<-NULL
  subs<-1:n
  for (j in seq(n,2,by=-1)){
    mrmr<-numeric(n)+Inf
    for (i in subs)
      mrmr[i]<-(1-lambda)*Iy[i]+lambda*mean(-Ix[setdiff(subs,i),i]+Ixx[setdiff(subs,i),i])

    s<-which.min(mrmr)
    nsubs<-c(s,nsubs)
    subs<-setdiff(subs,s)

  }

  subs<-c(subs,nsubs)

  if (back){
    nsubs<-NULL
    while (length(subs)>1){
      pd<-numeric(length(subs))

      for (ii in 1:length(subs))
        pd[ii]<-regrlin(X[,setdiff(subs,subs[ii])],Y)$MSE.emp
      nsubs<-c(subs[which.min(pd)],nsubs)
      subs<-setdiff(subs,subs[which.min(pd)])


    }
    subs<-c(subs,nsubs)
  }
  subs

}



disr<-function(X,Y,nmax=5,back=TRUE){

  X<-scale(X)
  Y<-scale(Y)
  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  N<-NROW(X)


  subset<-1:n
  if (FALSE){
    pv<-ppears(c(CY),N+numeric(n))
    s<-sort(pv,decreasing=TRUE,index.return=TRUE)
    hw<-length(which(s$x>0.2))
    subset<-setdiff(1:n,s$ix[1:hw])


    X<-X[,subset]

    n<-NCOL(X)
  }


  Inter<-array(NA,c(n,n))


  max.kj<--Inf
  for (kk in 1:(n-1)){
    for (jj in (kk+1):n){
      Inter[kk,jj]<- -var2H(regrlin(X[,c(kk,jj)],Y)$MSE.loo)+var2H(regrlin(X[,kk],Y)$MSE.loo)+var2H(regrlin(X[,jj],Y)$MSE.loo)
      Inter[jj,kk]<-Inter[kk,jj]
      if (Inter[kk,jj]>max.kj){
        max.kj<-Inter[kk,jj]
        subs<-c(kk,jj)
      }
    }
  }
  mrmr<-array(0,c(n,m))


  for (j in length(subs):min(n-2,nmax)){

    if (length(subs)<(n-1) & length(subs)>1){
      for (i in 1:m)
        mrmr[-subs,i]<- apply(Inter[subs,-subs],2,mean)
    } else {
      for (i in 1:m)
        mrmr[-subs,i]<- mean(Inter[subs,-subs])

    }

    pd<-numeric(n)-Inf
    if (m>1){
      pd[-subs]<-apply(mrmr[-subs,],1,mean)
    } else {
      pd[-subs]<-mrmr[-subs,1]
    }

    s<-which.max(pd)
    subs<-c(subs,s)

  }


  if (back){
    nsubs<-NULL
    while (length(subs)>1){
      pd<-numeric(length(subs))

      for (ii in 1:length(subs))
        pd[ii]<-regrlin(X[,setdiff(subs,subs[ii])],Y)$MSE.emp

      nsubs<-c(subs[which.min(pd)],nsubs)
      subs<-setdiff(subs,subs[which.min(pd)])


    }
    subs<-c(subs,nsubs)
  }



  subset[subs]


}

back<-function(X,Y,nmax){

  n<-NCOL(X)
  subs<-1:n

  nsubs<-NULL
  while (length(subs)>1){
    pd<-numeric(length(subs))

    for (ii in 1:length(subs))
      pd[ii]<-regrlin(X[,setdiff(subs,subs[ii])],Y)$MSE.loo
    nsubs<-c(subs[which.min(pd)],nsubs)
    subs<-setdiff(subs,subs[which.min(pd)])


  }

  subs<-c(subs,nsubs)
  subs[1:nmax]

}


varbool<-function(X,Y){
  if (levels(factor(Y))>2)
    stop("Error in varbool")

  n<-NCOL(X)

  if (n==1){
    p0<-which(Y==0)
    return(p0*(1-p0))
  }
  N<-NROW(X)
  w<-NULL
  V<-NULL
  for (j in (0:(2^n-1))){
    s<-unlist(strsplit(int2bit(j,n),""))
    ind<-1:N
    for (k in 1:n)
      ind<-intersect(ind,which(X[,k]==as.integer(s[k])))

    w<-c(w,length(ind)/N)
    p0<-which(Y[ind]==0)
    V<-c(V,p0*(1-p0))
  }

  as.numeric(V*w)



}

wrapbool<-function(X,Y,nmax=5){


  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  N<-NROW(X)

  VY<-numeric(n)
  for (i in 1:n)
    VY[i]<-varbool(X[,i],Y)

  subs<-which.min(VY)
  if (nmax==1)
    return(subs)

  for (j in length(subs):(nmax-1)){
    subs<-subs[1:j]
    pd<-numeric(n)+Inf
    for (i in setdiff(1:n,subs))
      pd[i]<-varbool(X[,c(subs,i)],Y)

    s<-which.min(pd)
    subs<-c(subs,sort(pd,decr=FALSE,ind=TRUE)$ix[1:(n-length(subs))])

  }


  subs<-subs[1:nmax]


}
sortrank2<-function(sx,rnk=FALSE){

  sx[which(is.nan(sx) | is.infinite(sx))]<-0
  sx[which(sx<quantile(sx,0.99))]<-0
  return(sx)
}

apply.thr<-function(x,rem=20){
  n<-length(x)
  s<-sort(x,decr=FALSE,ind=TRUE)$ix
  x[s[1:(n-rem)]]<-0
  x
}


net.inf<-function(X,Y=NULL,whichnodes=1:NCOL(X),
                  nmax=round(NCOL(X)/2),nit=35,
                  thr=0.5,filt="mrmr",meth="fw",verbose=FALSE,sparse=FALSE){
  ## meth= fw forward
  ## meth= ln linear
  # Adj[i,j] strength link i->j


  require(corpcor)
  n<-NCOL(X)
  N<-NROW(X)

  CX<-cor.shrink(X,lambda=thr,verbose=FALSE)
  ## CX<-cor(X)
  if (meth=='rank'){
    return(abs(CX))
  } ##  if (meth=='rank')

  C<-(cor2I2(CX))

  b<-C
  diag(b)<-0
  L<-min(nmax,n-1)
  W<-array(-Inf,c(n,n))
  AA<-C

  diag(AA)<-Inf
  if (sparse){
    for (i in 1:n){

      sA<-sort(abs(c(AA[,i])),decreasing=T,ind=T)$ix
      AA[sA[min(n,10):length(sA)],i]<-0

    }
  }
  diag(AA)<-NA
  diag(AA)<-max(apply(abs(AA),1,sum,na.rm=T))

  AA2<--AA
  diag(AA2)<-0
  if (sparse){
    AA<-as.simple_triplet_matrix(AA[,])
    AA2<-as.simple_triplet_matrix(AA2[,])
  }


  for (i in whichnodes){ ###  for (i
    if (verbose)
      print(i)
    wh<-sort(C[i,],dec=T,ind=T)$ix

    ind.i<-wh[2:(L+1)]
    A<-AA[ind.i,ind.i]
    A2<-AA2[ind.i,ind.i]
    b<-C[ind.i,i]
    ###### additional mimr code
    if (filt=="mimr"){
      cA<-CX[ind.i,ind.i]
      cB<-CX[ind.i,i]


      D<-sqrt(1-cB^2)%*%t(sqrt(1-cB^2))
      pC<-(cA-cB%*%t(cB))/D
      A3<-cor2I2(pC)

      A<- A-A3

    }
    ######

    if (meth=="ln"){
      ## Gradient method

      x<-numeric(L)+1/L

      prevx<-10*x
      it<-1
      ## gradient iteration

      while (mean(abs((x-prevx)))>1e-4 & it <nit ){
        if (sparse)
          b2<-tcrossprod_simple_triplet_matrix(A,t(x))
        else
          b2<-A%*%x
        r<-b-as.matrix(b2)

        alpha<-(t(r)%*%r)

        if (sparse)
          alpha<-alpha/(as.matrix(t(r)%*%tcrossprod_simple_triplet_matrix(A,t(r))))
        else
          alpha<-alpha/(as.matrix(t(r)%*%A%*%r))
        ##     alpha[which(is.na(alpha))]<-0
        prevx<-x

        newx<- x+as.numeric(alpha)*r
        it<-it+1

        newx[which(is.na(newx))]<-0
        if (sum(abs(newx))>0)
          x<-abs(newx)/sum(abs(newx))

      }


      W[ind.i,i]<-x

    }  ##  if (meth=="ln"){


    ####################
    if (meth=="ln2"){
      ## Jacobi method
      x<-numeric(length(b))+1/length(b)
      oldx<-x*0
      it<-1

      while (mean(abs((x-oldx)))>1e-4 & it <nit ){
        if (sparse)
          b2<-tcrossprod_simple_triplet_matrix(A2,t(x))
        else
          b2<-A2%*%x
        oldx<-x
        x<- (b2+ b)/10
        if (sum(abs(x))>0)
          x<-abs(x)/sum(abs(x))


        it<-it+1

      }

      W[ind.i,i]<-x

    }

    if (meth=="fw"){
      selected<-which.max(b)
      for (j in 2:nmax){
        notselected<-setdiff(1:L,selected)

        if (length(notselected)>1){
          if (length(selected)>1)
            selected<-c(selected,notselected[which.max(b[notselected]-apply(A[selected,notselected],2,mean))])
          else
            selected<-c(selected,notselected[which.max(b[notselected]-A[selected,notselected])])
        } else
          selected<-c(selected,notselected)

      }


      x<-seq(nmax,1,by=-1)
      W[ind.i[selected],i]<-x
    } ##  if (meth=="fw"){



  } ## for i

  return(W)


}



net.inf.sup<-function(X,Y,
                      nmax=NCOL(X),nit=15,nmax.fw=10,lambda=0.5,
                      thr=0.5,filt="mrmr",meth="fw",verbose=FALSE,sparse=FALSE){

  n<-NCOL(X)
  N<-NROW(X)

  CX<-cor(X) ##.shrink(X,lambda=thr,verbose=FALSE)
  C<-(cor2I2(CX))

  L<-min(nmax,n)
  S<-NULL
  if (is.vector(Y)){
    m<-1
  } else {
    m<-NCOL(Y)
  }

  W<-array(-Inf,c(n,m))
  cXY<-array(NA,c(n,m))

  AA<-C
  diag(AA)<- 0

  if (sparse){
    sA<-sort(abs(c(AA)),decreasing=T)
    AA[which(abs(AA)<sA[1000*n],arr.ind=TRUE)]<-0
  }

  diag(AA)<-NA
  diag(AA)<-1.1*max(c(apply(abs(AA),1,sum,na.rm=T),apply(abs(AA),2,sum,na.rm=T)))



  AA2<--AA
  diag(AA2)<-0
  if (sparse){
    AA<-as.simple_triplet_matrix(AA[,])
    AA2<-as.simple_triplet_matrix(AA2[,])
  }


  for (i in 1:m){
    if (verbose)
      print(i)
    if (m>1)
      YY<-Y[,i]
    else
      YY<-Y

    if (is.factor(YY)){
      l<-levels(YY)
      if (length(l)==2){
        yy<-numeric(N)
        ind1<-which(Y==l[1])
        ind2<-setdiff(1:N,ind1)
        yy[ind1]<-1
        YY<-yy
        cXY[,i]<-cor(X,yy)
      }
    } else {
      cXY[,i]<-cor(X,YY)
    }

    b<-cor2I2(cXY[,i])
    wh<-sort(b,dec=T,ind=T)$ix
    ind.i<-wh[1:(L)]

    A<-AA[ind.i,ind.i]
    A2<-AA2[ind.i,ind.i]
    b<-b[ind.i]


    ###### additional mimr code
    if (filt=="mimr"){
      cA<-CX[ind.i,ind.i]
      cB<-cXY[ind.i,i]

      D<-sqrt(1-cB^2)%*%t(sqrt(1-cB^2))
      pC<-(cA-cB%*%t(cB))/D
      A3<-cor2I2(pC)
      A<- A-A3


    }
    ####################

    b<-(1-lambda)*b
    A<-lambda*A

    if (meth=="ln"){
      ## Gradient method
      x<-b*0+1/length(b)
      prevx<-100+x
      it<-1
      ## gradient iteration

      while (mean(abs((x-prevx)))>1e-4 & it <nit ){
        if (sparse)
          b2<-tcrossprod_simple_triplet_matrix(A,t(x))
        else
          b2<-A%*%x
        r<-b-as.matrix(b2)

        alpha<-(t(r)%*%r)

        if (sparse)
          alpha<-alpha/(as.matrix(t(r)%*%tcrossprod_simple_triplet_matrix(A,t(r))))
        else
          alpha<-alpha/(as.matrix(t(r)%*%A%*%r))

        prevx<-x

        newx<- x+as.numeric(alpha)*r
        it<-it+1

        newx[which(is.na(newx))]<-0
        if (sum(abs(newx))>0)
          x<-abs(newx)/sum(abs(newx))

      }
      W[ind.i,i]<-x


    }  ##  if (meth=="ln"){

    if (meth=="ln2"){
      ## Jacobi method
      x<-b*0+1/length(b)

      oldx<-x*0
      it<-1
      while (it<=10){
        if (sparse)
          b2<-tcrossprod_simple_triplet_matrix(A2,t(x))
        else
          b2<-A2%*%x
        x<- (b2+ b)/10
        if (mean(abs(oldx-x))<1e-4)
          break
        if (sum(abs(x))>0)
          x<-abs(x)/sum(abs(x))
        oldx<-x
        it<-it+1
      }

      W[ind.i,i]<-x

    }
    if (meth=="fw"){
      selected<-which.max(b)
      for (j in 2:nmax.fw){
        notselected<-setdiff(1:L,selected)

        if (length(notselected)>1){
          if (length(selected)>1)
            selected<-c(selected,notselected[which.max(b[notselected]-apply(A[selected,notselected],2,mean))])
          else
            selected<-c(selected,notselected[which.max(b[notselected]-A[selected,notselected])])
        } else
          selected<-c(selected,notselected)
      }
      x<-seq(nmax.fw,1,by=-1)
      W[ind.i[selected],i]<-x
    } ##  if (meth=="fw"){
  } ## for (in in 1:m

  return(W)
}


netpred<-function(X,Y,Xts,filt,meth,thr=0,verbose=F){

  N<-length(Y)
  Nts<-NROW(Xts)
  lY<-levels(Y)
  ind0<-which(Y==lY[1])
  ind1<-which(Y==lY[2])

  p0<-length(ind0)/N
  p1<-length(ind1)/N

  print(system.time(n0<-net.inf(X[ind0,1:200],filt=filt,meth=meth,thr=thr,nmax=100,verbose=verbose)))
  n1<-net.inf(X[ind1,1:200],filt=filt,meth=meth,thr=thr,nmax=100,verbose=verbose)
  print("Inference done")
  y.hat<-NULL


  q<-Xts
  qq0<-q
  qq1<-q
  newqq0<-q
  newqq1<-q
  Q0<-NULL
  Q1<-NULL
  L0<-log(p0)+numeric(Nts)
  L1<-log(p1)+numeric(Nts)
  for (r in 1:50){
    for (j in 1:20){
      pred0<-sort(n0[,j],d=T,ind=T)$ix


      for (jj in 3){
        R<-regrlin(X[ind0,pred0[1:jj]],X[ind0,j],qq0[,pred0[1:jj]])

      }
      newqq0[,j]<-R$Y.hat.ts
      if (r>10)
        for (i in 1:Nts)
          L0[i]<-L0[i]+dnorm(newqq0[i,j],q[i,j],log=TRUE,sd=sqrt(R$MSE.loo))
      pred1<-sort(n1[,j],d=T,ind=T)$ix
      for (jj in 3){
        R<-regrlin(X[ind1,pred1[1:jj]],X[ind1,j],qq1[,pred1[1:jj]])
      }
      newqq1[,j]<-R$Y.hat.ts
      if (r>10)
        for (i in 1:Nts)
          L1[i]<-L1[i]+dnorm(newqq1[i,j],q[i,j],log=TRUE,sd=sqrt(R$MSE.loo))
    }
    qq1<-newqq1
    qq0<-newqq0
    Q0<-rbind(Q0,qq0)
    Q1<-rbind(Q1,qq1)

  }

  for (i in 1:Nts)
    y.hat<-c(y.hat,as.character(lY[which.max(c(L0[i],L1[i]))]))


  factor(y.hat,levels=lY)
}




hiton<-function(X,Y,algo=1){
  ## algo= 1 IAMB
  ## algo= 2 interIAMB
  ## algo= 3 IAMB + PC

  dir=paste(getwd(),round(sum(abs(X))+sample(1:1000000,1)),sep="/")

  XX<-cbind(X,Y)
  outp<-NCOL(XX)
  if (! file.exists(dir)) {
    dir.create(dir)
  } else {
    dir=paste(getwd(),round(sum(abs(Y))+sample(1:100000,1)),sep="/")
    dir.create(dir)
  }

  ## print(dir)
  # write(dir,file="dir.txt")

  filealgo<-paste(dir,"algo.txt",sep="/")
  write(algo,file=filealgo)

  filedata<-paste(dir,"data.txt",sep="/")
  write(t(XX),file=filedata,ncol=NCOL(XX))

  filevar<-paste(dir,"vari.txt",sep="/")
  write(outp,file=filevar,ncol=1)

  mat.cmd<-paste("matlab -nosplash -r \" causal2('" ,dir, "')\" < /dev/null > ",dir,"/log.out",sep="")

  print(mat.cmd)
  system(mat.cmd)
  Sys.sleep(0.1)

  filemb<-paste(dir,"mb.txt",sep="/")
  ftr.mb<-unique(unlist(read.table(filemb,sep="")))
  Sys.sleep(0.01)

  rmcmd<-paste("rm -r ",dir)
  system(rmcmd)

  ftr.mb



}


fastCI<-function(X,Y,i,coset,NC=20,L=max(5,min(20,NROW(X)/2))){
  n<-NCOL(X)
  N<-NROW(X)

  X<-scale(X)
  Y<-as.numeric(Y)
  set.seed(sum(coset))
  ## COND<-array(rnorm(NC*n,sd=0.1),c(NC,n))
  NC<-min(NC,N)
  COND<-X[sample(1:N,NC),]
  x<-i

  PV<-NULL
  subs<-coset
  for ( l in L:(L)){
    for (i in 1:NC){
      Xc<-X-array(1,c(N,1))%*%COND[i,]

      if (length(subs)==0){

        Ic<-sort(abs(Xc[,x]),decr=FALSE,index=TRUE)$ix[1:l]
      } else {
        if (length(subs)>1)
          Ic<-sort(apply(abs(Xc[,subs]),1,sum),decr=FALSE,index=TRUE)$ix[1:l]
        else
          Ic<-sort(abs(Xc[,subs]),decr=FALSE,index=TRUE)$ix[1:l]
      }


      e<-NULL
      e<-regrlin(X[Ic,x],Y[Ic])$MSE.loo
      ##for (ii in Ic){

      ##  e<-c(e,
      ##       pred("lazy",X[setdiff(Ic,ii),x],Y[setdiff(Ic,ii)],X[ii,x],conPar=c(3,10),linPar=NULL,class=FALSE)-Y[ii])

      ## }
      ## e<-c(e,
      ##   pred("lin",X[setdiff(Ic,ii),x],Y[setdiff(Ic,ii)],X[ii,x],class=FALSE)-Y[ii])


      PV<-c(PV,-mean(e^2))



    } ## for i
  }# for l



  mean(PV)
}

selCI<-function(X,Y,nmax=5){

  w<-which(apply(X,2,sd)>0)
  X<-X[,w]
  n<-NCOL(X)
  subs<-which.max(abs(cor(X,as.numeric(Y))))

  for (j in 2:(nmax)){

    pd<-numeric(n)-Inf
    for (i in setdiff(1:n,subs)){

      pd[i]<-fastCI(X,Y,i,subs,NC=100)

    }

    subs<-c(subs,which.max(pd))



  }

  return(w[subs])


}

filtdiff<-function(X,Y,nmax=5,nmax2=NCOL(X),back=FALSE){

  subset<-which(apply(X,2,sd)>0)
  X<-X[,subset]

  X<-scale(X)
  Y<-as.numeric(Y)
  Y<-scale(Y)
  m<-NCOL(Y) # number of outputs
  n<-NCOL(X)
  N<-NROW(X)

  NMAX<-min(nmax,nmax2)
  CY<-corXY(X,Y)


  subs<-which.max(abs(CY))

  for (j in length(subs):NMAX){
    subs<-subs[1:j]
    pd<-numeric(n)+Inf
    for (i in setdiff(1:n,subs)){
      d<-NULL

      if (FALSE){
        for (r in 1:2){
          Itr<-sample(1:N,round(2*N/3))
          Its<-setdiff(1:N,Itr)

          ed<- Y[Its]-pred("lazy",X[Itr,c(subs,i)],Y[Itr],X[Its,c(subs,i)],
                           linPar=NULL,class=FALSE)

          ie<-Y[Its]-1/2*(pred("lazy",X[Itr,subs],Y[Itr],X[Its,subs],
                               linPar=NULL,class=FALSE)+pred("lazy",X[Itr,i],Y[Itr],X[Its,i],
                                                             linPar=NULL,class=FALSE))
          d<-c(d,mean(ed^2)/var(Y[Its])+abs(mean(ed^2)/var(Y[Its])- mean(ie^2)/var(Y[Its])))

        }
      } else {
        for (r in 1:10){
          Itr<-sample(1:N,round(2*N/3))
          Its<-setdiff(1:N,Itr)
          ed<- Y[Its]-pred("lin",X[Itr,c(subs,i)],Y[Itr],X[Its,c(subs,i)],
                           class=FALSE)

          ie<-Y[Its]-1/2*(pred("lin",X[Itr,subs],Y[Itr],X[Its,subs],
                               class=FALSE)+pred("lin",X[Itr,i],Y[Itr],X[Its,i],
                                                 class=FALSE))
          d<-c(d,mean(ed^2)/var(Y[Its])- 0.5*mean(ie^2)/var(Y[Its]))
        }

      }

      pd[i]<-mean(d)
    }
    s<-which.min(pd)
    subs<-c(subs,sort(pd,decr=FALSE,ind=TRUE)$ix[1:(n-length(subs))])

  }




  subset[subs[1:nmax]]

}
