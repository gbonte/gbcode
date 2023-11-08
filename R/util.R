
nnls.fit <- function(x,y,wsqrt=1,eps=0,rank.tol=1e-07) {
  ## Purpose: Nonnegative Least Squares (similar to the S-Plus function
  ## with the same name) with the help of the R-library quadprog

  ## ------------------------------------------------------------------------
  ## Attention:
  ## - weights are square roots of usual weights
  ## - the constraint is coefficient>=eps

  ## ------------------------------------------------------------------------
  ## Author: Marcel Wolbers, July 99
  ##
  ##========================================================================
  require ("quadprog")
  m <- NCOL(x)
  if (length(eps)==1) eps <- rep(eps,m)
  x <- x * wsqrt
  y <- y * wsqrt
  #sometimes a rescaling of x and y helps (if solve.QP.compact fails   otherwise)
  xscale <- apply(abs(x),2,mean)
  yscale <- mean(abs(y))
  x <- t(t(x)/xscale)
  y <- y/yscale
  Rinv <- backsolve(qr.R(qr(x)),diag(m))
  cf <- solve.QP.compact(Dmat=Rinv,dvec=t(x)%*%y,Amat=rbind(rep(1,m)),
                         Aind=rbind(rep(1,m),1:m),bvec=eps*xscale/yscale,
                         factorized=TRUE)$sol

  cf <- cf*yscale/xscale  #scale back
  cf
}

wmean<-function(x,w){
  sum(x*w)


}

uniscale<-function(x,ord=FALSE){

  if (ord)
    x<-order(x)
  (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))

}
any.nan<-function(x){
  any(is.nan(x))| any(is.na(x))
}


ret.rnd<-function(){
  tt<-Sys.time()
  as.integer(as.numeric(system("date +%N",intern=TRUE))*as.numeric(system("date +%N",intern=TRUE))/1e10)
  #as.numeric(format(tt,"%S"))*as.numeric(format(tt,"%M"))*as.numeric(format(tt,"%H"))*as.numeric(format(tt,"%d"))
}

code<-function(v){
  C<-0
  for (i in 1:length(v))
    C<-C+v[i]*10^(i-1)

  C
}

decode<-function(x,size){
  X<-as.character(x)


  L<-nchar(X)
  if (L<size)
    for (i in (L+1):size)
      X<-paste('0',X,sep="")

  D<-NULL
  for (l in 1:size)
    D<-c(D,as.integer(substr(X,l,l)))

  D
}

which.min.mat<-function(W,diag=TRUE){
  if (!diag)
    diag(W)<-Inf
  I<-which(W==min(W),arr.ind=TRUE)
  I[1,]
}



#' imputeDataset
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{https://tinyurl.com/sfmlh}
#' @description Impute data set by removing constant columns and replacing NA samples with the mean
#' @details Impute data set by removing constant columns and replacing NA samples with the mean
#' @title impute
#' @name impute
#' @export
#'
#' @param  X: dataset
#' @return A list with:
#' \itemize{
#' \item{impX:} imputed dataset
#' \item{remcol:} indices of remaining columns
#' }
#' @export

imputeDataset<-function(X){
  remcol<-NULL
  N<-NROW(X)
  for (i in 1:NCOL(X)){
    w<-which(is.na(X[,i]))
    if (length(w)==N) {
      remcol<-c(remcol,i)
    } else {
      if (length(w)>0)
        X[w,i]<-mean(X[,i],na.rm=TRUE)

      if (sd(X[,i],na.rm=TRUE)<1e-2)
        remcol<-c(remcol,i)
    }
  }

  remcol<-unique(remcol)
  if (length(remcol)>0)
    X<-X[,-remcol]
  list(impX=X,remcol=remcol)

}





normalize<-function(x){
  (x-min(x))/(max(x)-min(x))


}
balanced.index<-function(YY,verbose=T){
  if (! is.factor(Y))
    stop("YY is not a factor")
  l.y<-levels(YY)
  how.many.l<-numeric(length(l.y))
  which.levels<-list()
  for (i in 1:length(l.y)){
    which.levels<-c(which.levels,list(which(YY==l.y[i])))
    how.many.l[i]<-length(which(YY==l.y[i]))
  }

  Ib<-NULL
  for (i in 1:length(how.many.l)){
    W<-which.levels[[i]]
    Ib<-c(Ib,sample(W,min(how.many.l)))
  }
  Ib<-sample(Ib)
  if (verbose)
    cat("Size dataset reduced of ", 100*(1-length(Ib)/length(YY)), "% \n")
  return(Ib)
}





equal<-function(a,b){
  if (length(a)!=length(b))
    return(FALSE)
  all(sort(a)==sort(b))
}

most.freq<-function(x,howmany=1,w=numeric(length(x))+1,u=NULL){
  if (is.null(u))
    u<-unique(x)
  c.u<-numeric(length(u))
  for (i in 1:length(u))
    c.u[i]<-sum(as.numeric(x==u[i])*w)
  s<-sort(c.u,index.return=T,decreasing=T)
  u[s$ix[1:min(length(u),howmany)]]
}


which.equal<-function(x,a){
  which(x==a)
}


which.freq<-function(x,u=NULL){
  if (is.null(u))
    u<-unique(x)
  c.u<-numeric(length(u))
  for (i in 1:length(u))
    c.u[i]<-sum(x==u[i])
  c.u/length(x)
}

intersect.all<-function(li){
  L<-length(li)
  if (L==1)
    return(li[[1]])
  inters<-li[[1]]
  i<-2
  while (length(inters)>0 & i<=L){
    inters<-intersect(inters,li[[i]])

    i<-i+1
  }
  inters
}


preproc<-function(X,remove.nan=TRUE,remove.const=TRUE,
                  to.scale=TRUE,which.rem=FALSE,verbose=F){


  w.na<-(which(apply(X,2,any.nan)))
  ind.rem<-1:NCOL(X)
  if (remove.nan & length(w.na)>0){
    X<-X[, -w.na]
    ind.rem<-setdiff(ind.rem,w.na)
    if (verbose)
      cat("\n Removed", length(w.na),"  NA columns \n")
  }

  if (remove.const){
    ind.Sd<-which(apply(X,2,sd)>0)
    if (length(ind.Sd)<NCOL(X) & verbose)
      cat("\n Removed", NCOL(X)-length(ind.Sd),"  constant columns \n")
    X<-X[,ind.Sd]
    ind.rem<-ind.rem[ind.Sd]
  }

  if (to.scale)
    X<-scale(X)
  if (!which.rem)
    return(X)
  else
    return(list(X=X,ind.rem=ind.rem))

}


#' preproc2
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} 
#' @description Preprocesses a training and a test dataset
#' @details It performs hierarchical clustering to reduce the number of features
#' @title preproc2
#' @name preproc2
#' @export
#'
#' @param  X1: training dataset
#' @param  X2: test dataset
#' @param  remove.nan: TRUE/FALSE. It removes NaN
#' @param  remove.const: TRUE/FALSE. It removes constant columns.
#' @param  to.scale: TRUE/FALSE. It normalizes columns
#' @param  cluster: Number of feature clusters. If =0 it does nothing.
#' @return A list with:
#' \itemize{
#' \item{X1:} preprocessed training dataset
#' \item{X2:} preprocessed training dataset
#' \item{remcol:} indices of remaining columns
#' \item{groups:} indices of clusters components
#' }
#' @export
#' 
preproc2<-function(X1,X2,remove.nan=TRUE,remove.const=TRUE,to.scale=TRUE,verbose=T,cluster=0){
  groups<-NULL
  ind.rem<-1:NCOL(X1)
  if (to.scale){
    X1<-scale(X1)

    X2<-scale(X2,center=attr(X1,"scaled:center"),
              scale=attr(X1,"scaled:scale"))
  }

  w.na<-union(which(apply(X1,2,any.nan)),which(apply(X2,2,any.nan)))
  if (length(w.na)>0)
    if (remove.nan){
      X1<-X1[, -w.na]
      X2<-X2[,-w.na]
      ##   X1<-impute(X1)
      ##  X2<-impute(X2)
      ##  X1[, w.na]<-rnorm(NROW(X1),sd=0.01)
      ##  X2[, w.na]<-rnorm(NROW(X2),sd=0.01)

      ind.rem<-setdiff(ind.rem,w.na)
      if (verbose)
        cat("\n Removed", length(w.na),"  NA columns \n")
    } else {
      require(impute)
      X1<-t(impute.knn(t(X1),colmax =1,rowmax=0.9))
      X2<-t(impute.knn(t(X2),colmax=1,rowmax=0.9))

    }



  if (remove.const){
    ind.Sd<-intersect(which(apply(X1,2,sd)>0),which(apply(X2,2,sd)>0))
    if (length(ind.Sd)<NCOL(X1) & verbose)
      cat("\n Removed", NCOL(X1)-length(ind.Sd),"  constant columns \n")
    X1<-X1[,ind.Sd]
    X2<-X2[,ind.Sd]
    ind.rem<-ind.rem[ind.Sd]
  }



  if (cluster >0){

    max.n<-5000
    if (NCOL(X1)>max.n){

      r<-ceiling(NCOL(X1)/max.n)
      X1c<-NULL
      X2c<-NULL

      for (i in 1:(r)){
        print(i)
        ind.col<-((i-1)*max.n+1):min(c(NCOL(X1),(i*max.n)))
        cat("min(ind.col)=",min(ind.col),"max(ind.col)=",max(ind.col),"\n")
        if (length(ind.col)>round(max.n/r)){

          ##  dd <- dist(t(X1[,ind.col]))
          dd <- as.dist(1-(cor((X1[,ind.col]))))
          ff <- hclust(dd,"co")
          CL<-Clustering(ff,X1[,ind.col],NbCluster=min(length(ind.col),round(max.n/r)),pca=0,avg=T)
          X1c<-cbind(X1c,CL$XX)
          X2c<-cbind(X2c,Clustering(ff,X2[,ind.col],NbCluster=round(max.n/r),pca=0,avg=T)$XX)
          groups<-c(groups,list(CL$groups))

        } else {
          X1c<-cbind(X1c,X1[,ind.col])
          X2c<-cbind(X2c,X2[,ind.col])

        }


      } ## for i


      X1<-X1c
      X2<-X2c

    } ## if (NCOL
    print(dim(X1))
    ##  dd <- dist(t(X1))
    dd <- as.dist(1-abs(cor((X1))))
    ##  browser()
    ff <- hclust(dd,"co")
    CL<-Clustering(ff,X1,NbCluster=cluster,pca=0,avg=T)
    X1<-CL$XX




    groups<-c(groups,list(CL$groups))
    X2<-Clustering(ff,X2,NbCluster=cluster,pca=0,avg=T)$XX
    if (verbose)
      cat("\n Clustered in ", cluster," columns \n")
  }

  if (length(groups)>1){
    clu<-groups
    clu2<-numeric(NCOL(X1))
    lastclu<-clu[[length(clu)]]
    cnt<-1
    for (ii in 1:length(clu))
      for (iii in 1:length(clu[[ii]])){
        clu2[cnt]<-lastclu[clu[[ii]][iii]+(ii-1)*round(max.n/r)]
        cnt<-cnt+1
      }

    groups<-list(clu2)
  }
  list(X1=X1,X2=X2,ind.rem=ind.rem,groups=groups)

}


preprochip<-function(X,fact=F){ ## X[no.genes,no.conditions]
  source("/home/datamining/code/util_chip_standardization.R")

  D<-standardize.chips(X)
  if (fact)
    DN2<-data.frame(apply(D$E.value<1,2,as.character))
  else
    DN2<-D$z
  rownames(DN2)<-rownames(X)
  colnames(DN2)<-colnames(X)
  DN2
}

ana<-function(x){
  any(is.na(x))
}

which.na<-function(x){
  which(is.na(x))
}


man.miss<-function(X,verbose=F){
  if (is.vector(X)){
    w<-which(is.na(X))
    mu<-mean(X,na.rm=T)
    X[w]<-mu
  } else{
    mu<-apply(X,2,mean,na.rm=T)
    w<-apply(X,2,which.na)
    for (i in 1:NCOL(X)){

      X[w[[i]],i]<-mu[i]
      if (verbose)
        print(i)
    }
  }
  X
}


scaling<-function(data){


  no.data<-NROW(data)
  no.var<-NCOL(data)
  scale.data<-array(0,c(no.data,no.var))
  A.max<-1
  A.min<--1

  V.max<-1.28
  # Z-score max limit

  V.min<--1.28
  #  Z-score min limit

  if (no.var>1){
    mean.data<-apply(data,2,mean)
    std.data<-apply(data,2,sd)
  } else
  {
    mean.data<-mean(data)
    std.data<-sd(data)
  }

  r<-(A.max-A.min)/(V.max-V.min)

  if (no.var>1){
    for (i in 1:no.var){
      scale.data[,i]=r*((data[,i]-mean.data[i])/std.data[i]-V.min)+A.min
    }
  } else
    scale.data=r*((data-mean.data)/std.data-V.min)+A.min

  list(scale.data=scale.data,mean.data=mean.data,std.data=std.data)

}


scaling2<-function (data,mean.data,std.data){

  no.data<-NROW(data)
  no.var<-NCOL(data)
  scale.data<-array(0,c(no.data,no.var))

  A.max<-1
  A.min<--1

  V.max<-1.28
  # Z-score max limit

  V.min<--1.28
  #  Z-score min limit



  r=(A.max-A.min)/(V.max-V.min);
  if (no.var>1){
    for (i in 1:no.var){
      scale.data[,i]=r*((data[,i]-mean.data[i])/std.data[i]-V.min)+A.min;
    }
  } else
    scale.data<-r*((data-mean.data)/std.data-V.min)+A.min;

  scale.data
}


unscalin<- function(scale.data,mean.data,std.data){


  no.data<-NROW(scale.data)
  no.var<-NCOL(scale.data)

  A.max<-1
  A.min<--1

  V.max<-1.28
  V.min<--1.28

  r=(A.max-A.min)/(V.max-V.min);

  if (no.var>1){
    for (i in 1:no.var){
      data[,i]=(std.data[i]*scale.data[,i])/r+(mean.data[i]+std.data[i]*(V.min-A.min/r))
    }
  } else
    data<-(std.data*scale.data)/r+(mean.data+std.data*(V.min-A.min/r))

  data
}


unscale<-function(sX){
  return(t(apply(sX, 1, function(r)r*attr(sX,'scaled:scale') + attr(sX, 'scaled:center'))))
  
}

unscale2<-function(sXhat,sX){
  return(t(apply(sXhat, 1, function(r)r*attr(sX,'scaled:scale') + attr(sX, 'scaled:center'))))
  
}

normv<-function(x,p=2){
  sum(x^p)^(1/p)

}

normm<-function(A,p=2){
  E<-eigen(t(A)%*%A)
  e<-max(abs(E$values))
  sqrt(e)

}



varM<-function(X){
  n<-NCOL(X)
  N<-NROW(X)
  xm<-apply(X,2,mean)
  t(X)%*%X/(N-1)-xm%*%t(xm)

}

wrank<-function(x,e){
  which(x==e)
}

rank.subelement.list<-function(e,l){
  unlist(lapply(l,wrank,e))

}

#' which.element.list
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description Return index list equal to a a given element
#' @details Return index of list whose content is equal to a a given element
#' @title which.element.list
#' @name which.element.list
#' @export
#'
#' @param  e: element
#' @param l: list
#' @examples
#' L<-list("aa","b","a1","a","E")
#' which.element.list("a",L)
#'
which.element.list<-function(e,l){
  if (length(l)<1)
    return(NULL)
  which(unlist(lapply(l,equal,e)))
}

#' is.element.list
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description Return TRUE if element belongs to list
#' @details Return TRUE if element belongs to list
#' @title is.element.list
#' @name is.element.list
#' @export
#'
#' @param  e: element
#' @param l: list
#' @examples
#' L<-list("aa","b","a1","a","E")
#' is.element.list("a",L)
is.element.list<-function(e,l){
  any(unlist(lapply(l,equal,e)))
}




is.subset<-function(e2,e1){
  length(intersect(e1,e2))==length(e1)
}

#' which.subelement.list
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description Returns indices of list terms xhich contain a given element
#' @details Returns indices of list terms xhich contain a given element
#' @title which.subelement.list
#' @name which.subelement.list
#' @export
#'
#' @param  e: element
#' @param l: list
#' @examples
#' L<-list("aa","b","a1","a","E")
#' L[[6]]<-c("a","z")
#' which.subelement.list("a",L)
#'
which.subelement.list<-function(e,l){
  w<-which(unlist(lapply(lapply(l,is.subset,e),any)))
  if (length(w)==0)
    return(NA)
  else
    return(w)
}


howmany.subelement.list<-function(e,l){
  sum(unlist(lapply((lapply(l,is.element,e)),any)))

}

entropy2<-function(x){
  V<-unique(x)
  N<-length(x)
  n<-length(V)
  if (n==1)
    return(0)
  p<-NULL
  for (i in 1:n)
    p<-c(p,length(which(x==V[i]))/N)

  entropy(p)
}

entropy<-function(p,m=0){

  if (abs(sum(p)-1)>1e-3){
    print(p)
    stop("error")
  }
  ##  n<-length(p)
  ##  if (F){
  ##    for (i in 1:n){
  ##      if (p[i]>0)
  ##        H<-H+p[i]*log2(p[i])
  ##
  ##    }
  ##    -H/log2(n)
  ##  }
  H<-1-sum(p^2)

}


ppermtest<-function(x,y,R=1000){
  N<-length(x)
  if (N!=length(y))
    stop("This is a paired test !")

  d<-x-y
  s<-mean(d)
  sr<-NULL
  for ( r in 1:R){
    I<-sample(c(-1,1),N,replace=TRUE)
    sr<-c(sr,mean(d*I))
  }
  p<-length(which(abs(sr)>abs(s)))/R

  p
}



#' hypergeometric pvalue
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description Returns pvalue of hypergeometric
#' @details Returns pvalue of hypergeometric
#' @title hypergeometric pvalue
#' @name hyperg
#' @export
#'
#' @param  m: number of marked elements
#' @param  n: number of unmarked elements
#' @param k: selection size
#' @param x: number of marked elements in our selection
#' @return p: pvalue. Probability of selecting a number of marked elements larger equal than x if the selection if random
#' @export
#' @examples
#' hyperg(10,5,2,1)
#'
hyperg<-function(m,n,k,x){
  p<-0
  for (i in x:min(k,m))
    p<-p+dhyper(i,m,n,k)
  p
}




## feature slection based on the notion of multicollinearity
multisel<-function(X,thr=0.8){
  require(MASS)
  n<-NCOL(X)
  m<-apply(X,2,mean)


  for (i in 1:NCOL(X))
    X[,i]<-(X[,i]-m[i])

  s2<-apply(X^2,2,sum)
  for (i in 1:NCOL(X))
    X[,i]<-X[,i]/sqrt(s2[i])

  delta<-1
  rem<-NULL
  for (i in seq(delta,n,by=delta)){
    var<-setdiff(1:i,rem)
    XX<-array(t(X[,var])%*%X[,var],c(length(var),length(var)))
    if (det(XX)>0){
      C<-ginv(XX)

      R2<-1-1/diag(C) ## Coeeficient of determination
      ##when an input is regressed on the remaining n-1 inputs

      if (max(R2)>thr){
        rem<-c(rem,var[which.max(R2)])
      }
    }else{
      rem<-c(rem,(i-delta+1):i)
    }
  }




  setdiff(1:n,rem)
}

ica<-function(X,n.comp){
  require(fastICA)
  N.tr<-NROW(X)
  icaX<-fastICA(X,fun="logcosh",n.comp=n.comp)
  list(data=icaX$S)
}


#' PCA
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description PCA compression
#' @details PCA compression
#' @title PCA compression
#' @name pca
#' @export
#'
#' @param  X: input data
#' @param  n.comp: number of returned principal components
#' @param  Xts: input test dataset
#' @return list
#' \itemize{
#' \item{data}: transformed data
#' \item{vect}: principal eigenvectors
#' \item{val}: principal eigenvalues
#' \item{ord}:
#' \item{mu}: mean of original data
#' \item{datats}: transformed test dataset
#' }
#' @export
#' @examples
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1)
#' X<-R$X
#' Z<-pca(X,3)$data
#'
pca<-function(X,n.comp=2,Xts=NULL){
  n<-NCOL(X)
  sX<-scale(X)
  
  mu<-apply(X,2,mean)
  E<-eigen(cov(sX),symmetric=T,only.values=FALSE)
  pca<-E$vectors
  val<-E$values

  nval<-cumsum(val)/sum(val)
  thr<-0.9
  if (nval[1]>thr) {
    ord<-1
  } else {
    if (nval[n-1]<thr){
      ord<-n
    } else {
      ord<-max(which(nval<thr))+1
    }
  }

  datats=NULL
  if (!is.null(Xts)){
    if (NCOL(X)!=NCOL(Xts))
      stop("Wrong Xts dimension")
    sXts=scale(Xts,attr(sX,"scaled:center"),attr(sX,"scaled:scale"))
    datats=sXts%*%pca[,1:n.comp]
  }
    
  list(data=sX%*%pca[,1:n.comp],vect=pca,val=E$values,ord=ord,mu=mu,datats=datats)
}




#' PCA by SVD decomposition
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description PCA compression by SVD decomposition
#' @details PCA compression by SVD decomposition
#' @title PCA compression by SVD decomposition
#' @name pca.svd
#' @export
#'
#' @param  X: input data
#' @param  n.comp: number of returned principal components
#' @return list
#' \itemize{
#' \item{data}: transformed data
#' \item{vect}: principal eigenvectors
#' \item{val}: principal eigenvalues
#' \item{ord}:
#' \item{mu}: mean of original data
#' }
#' @export
#' @examples
#' N<-100
#' n<-5
#' neff<-3
#' R<-regrDataset(N,n,neff,0.1)
#' X<-R$X
#' Z<-pca.svd(X,3)$data
#'
pca.svd<-function(X,n.comp=2){
  n<-NCOL(X)
  sX<-scale(X)
  mu<-apply(X,2,mean)
  S<-svd(sX)
  pca<-S$v

  ord<-min(n,max(which(cumsum(S$d)<(sum(S$d)*0.9)))+1)

  list(data=sX%*%pca[,1:n.comp],vect=pca[,1:n.comp],mu=mu,ord=ord)

}



samm<-function(X){
  X2<-sammon(dist(X))
}

lsa.alg<-function(X,n.comp=2){
  require(lsa,lib="/u/gbonte/libs")
  n.comp<-min(n.comp,NCOL(X))
  sX<-scale(X)
  XX<-lsa(as.textmatrix(t(sX)), dims=n.comp)$dk
  list(data=XX)
}



Clustering <- function(clust,X,NbCluster,pca=0,V=NULL,avg=T) {


  gg <- cutree(clust,k=min(NbCluster,NCOL(X)))
  XX<-NULL


  for (i in 1:NbCluster){
    ind<-which(gg==i)
    if (length(ind)>1){
      if (pca==1){
        PCA<-pca(X[,ind],n.comp=1)
        X2<-PCA$data
        V<-c(V,list(PCA$vect))
      }
      if (pca==0){
        if (avg)
          X2<-apply(X[,ind],1,mean)
        else
          X2<-apply(X[,ind],1,max)
      }
      if (pca==2){

        X2<-X[,ind]%*%V[[i]]
      }
    }else{
      X2<-X[,ind]
      if (pca==1)
        V<-c(V,list(1))
    }
    XX<-cbind(XX,X2)
  }

  list(XX=XX,groups=gg,V=V)
}


plaw.plot<-function(x,no.breaks=20,title=""){
  par(ask=T)
  HH<-hist(x,breaks=no.breaks,main="")
  py<-HH$counts/sum(HH$counts)
  px<-HH$mids
  ind<-which(px>0 & py>0)
  px<-px[ind]
  py<-py[ind]
  L<-lm(log10(py)~log10(px))
  plot(log10(px),log10(py))
  lines(log10(px),L$fitted.values)
  require(evd)
  E<-ecdf(x)
  rX<-seq(min(x),max(x),by=(max(x)-min(x))/100)
  plot(rX,E(rX),main="",xlim=c(min(x),max(x)),type="p")
  ml<- fpot(x,0,model="gpd",std.err=F)


  lines(rX,pgpd(rX,loc=0,scale=ml$estimate["scale"],shape=ml$estimate["shape"]),col="red")


}



pval.law2 <-function(X,R=10000){
  require(evd)
  N<-length(X)
  loc<-0
  ml<- fpot(X,loc,model="gpd",std.err=FALSE)

  if (F){
    K<-numeric(R)
    for (r in 1:R){
      sc<-sample(1:10,1)
      sh<-sample(1:10,1)
      x<-rgpd(N,loc=loc,scale=sc,shape=sh)
      mlr<- fpot(x,loc,model="gpd",std.err=FALSE)
      S<-ks.test(x,"pgpd",loc=loc,scale=mlr$estimate["scale"],
                 shape=mlr$estimate["shape"])$statistic
      K[r]<-as.numeric(S)
    }
  } else {
    load("/home/gbonte/R/kontos/pvalaw.Rdata")
  }

  SX<-ks.test(X,"pgpd",loc=loc,scale=ml$estimate["scale"],
              shape=ml$estimate["shape"])
  KX<-as.numeric(SX$statistic)
  PX<-as.numeric(SX$p.value)

  pval<-length(which(abs(K)>KX))/R

  list(pval=pval,D=KX,P=PX,ml=ml,K=K)
}




pval.law <-function(X,R=10000){
  require(evd)
  N<-length(X)
  loc<-0
  ml<- fpot(X,loc,model="gpd",std.err=FALSE)

  K<-numeric(R)
  for (r in 1:R){
    x<-rgpd(N,loc=loc,scale=ml$estimate["scale"],shape=ml$estimate["shape"])
    mlr<- fpot(x,loc,model="gpd",std.err=FALSE)
    S<-ks.test(x,"pgpd",loc=loc,scale=mlr$estimate["scale"],
               shape=mlr$estimate["shape"])$statistic
    K[r]<-as.numeric(S)
  }


  SX<-ks.test(X,"pgpd",loc=loc,scale=ml$estimate["scale"],
              shape=ml$estimate["shape"])
  KX<-as.numeric(SX$statistic)
  PX<-as.numeric(SX$p.value)

  pval<-length(which(abs(K)>KX))/R

  list(pval=pval,D=KX,P=PX,ml=ml,K=K)
}



plaw.plot2<-function(x,no.breaks=20){
  x<-x[which(x>0)]
  R<-length(x)
  sx<-sort(x)
  p<-(1:R)/R

  Y<-log(sx)
  X<-log(p)
  Y<-Y[1:(R-1)]
  X<-X[1:(R-1)]
  L<-lm(Y~X)

  1/(L$coef[2])
  summary(L)
  plot(X,Y)
  lines(X,L$fitted.values)
}



## Takes as input an adjacent matrix and produces a '.tgf' file.
## Parameters: an adjacent matrix and a filename (default is 'adjacent.tgf').

adjacent.to.tgf <- function(adj,filename="adjacent.tgf") {
  adj[which(is.na(adj))]<-0

  splt <- strsplit(filename,split="")[[1]]
  lg <- length(splt)
  if (!paste(splt[(lg-3):lg],collapse="")==".tgf") {
    filename <- paste(filename,".tgf",sep="")
  }
  n <- nrow(adj)

  for (i in 1:n) {
    cat(i," ",rownames(adj)[i],"\n",file=filename,sep="",append=TRUE)
  }
  cat("#","\n",file=filename,sep="",append=TRUE)
  for (i in 1:n) {
    for (j in 1:ncol(adj)) {
      if (adj[i,j] == 1) {
        cat(ind.col[j]," ",i,"\n",file=filename,sep="",append=TRUE)
      }
    }
  }
}


merge<-function(names1,names2){
  inters.1.2<-match(names1,names2)
  ind.not.na<-which(!is.na(inters.1.2))

  ind1<-ind.not.na
  ind2<-inters.1.2[ind.not.na]

  list(ind1=ind1,ind2=ind2)


}



ztrans<-function(p){

  z<-qnorm(p)
  pnorm(sum(z)/sqrt(length(z)))


}

dist.cos<-function(X1,X2){

  N1<-NROW(X1)
  n<-NCOL(X1)
  n2<-NCOL(X2)
  N2<-NROW(X2)


  if (n != n2){
    cat("\n n=",n)
    cat("\n n2=",n2)
    stop('Matrix sizes do not match.')
  }

  if (is.vector(X1))
    S1<-X1^2
  else
    S1<-apply(X1^2,1,sum)
  if (is.vector(X2))
    S2<-X2^2
  else
    S2<-apply(X2^2,1,sum)
  SS<-sqrt(S1%*%t(S2))

  D<-(X1%*%t(X2))/SS
  1-D

}


dist2<-function(X1,X2){
  ## X1 [N1,n]
  ## X2 [N2,n]


  N1<-NROW(X1)
  n<-NCOL(X1)
  n2<-NCOL(X2)
  N2<-NROW(X2)



  if (n != n2){
    cat("\n n=",n)
    cat("\n n2=",n2)
    stop('Matrix sizes do not match.')
  }

  y<-array(0,c(N1,N2))

  if (n==1){

    for (i in 1:N1){
      x <- array(1,c(N2,1))%*%as.numeric(X1[i])
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



  ## y[N1,N2]
  sqrt(y)

}



varestim<-function(X,Y,X.ts,N.ts=1000,B=100,class=T,algo,algoptions,seed=0){
  set.seed(seed)
  L<-length(levels(Y))
  N.ts<-NROW(X.ts)


  pred.B<-array(0,c(N.ts,L))
  colnames(pred.B)<-levels(Y)
  for ( b in 1:B){
    ind.tr<-sample(NROW(X),NROW(X),replace=T)
    p<-pred(algo,X[ind.tr, ],Y[ind.tr],X.ts[,],algoptions)
    pred.B[cbind(1:N.ts,p)]<-pred.B[cbind(1:N.ts,p)]+1

  }
  pred.B<-pred.B/B
  mean(apply(pred.B,1,entropy))



}


make.netw<-function(X){

  N<-NROW(X)
  n<-NCOL(X)
  Adj<-array(0,c(n,n))
  MSE<-numeric(n)
  for (i in 1:(n)){
    ind<-setdiff(1:n,i)
    G<-gsloo(X[,ind],X[,i],nmax=min(c(length(ind),NCOL(X)-1)),
             automatic=T,trace=F)
    sel<-ind[G$sel]
    Adj[i,sel]<-Adj[i,sel]+1
    ##Adj[sel,i]<-Adj[sel,i]+1
    MSE[i]<-G$Jloo

  }
  sum.conn<-apply(Adj,1,sum)
  rank.conn<-sort(sum.conn,decreasing=T,index.return=T)$ix

  list(Adj=Adj,rank.conn=rank.conn,J=MSE)
}


sel.netw<-function(X,Y){
  L<-levels(Y)
  n<-NCOL(X)
  r<-array(0,c(length(L),n))
  for (l in 1:length(L)){

    ind.l<-which(Y==L[l])
    if (length(ind.l)>5){
      rank.conn<-make.netw(X[ind.l,])$rank.conn

      r[l,rank.conn]<-1:n
    }
  }

  sum.rank.conn<-apply(r,2,sum)
  rank.conn<-sort(sum.rank.conn,index.return=T)$ix

  rank.conn

}



rand.ev<-function(Y){
  L<-levels(Y)
  no<-NULL
  for ( i in 1:length(L))
    no<-c(no,length(which(Y==L[i])))

  no<-no/sum(no)

  sum(no^2)



}



bias.min<-function(E,B=200){

  N<-NROW(E)
  n<-NCOL(E)

  mn<-min(apply(E,2,mean))

  mn.b<-NULL
  for (b in 1:B){
    ib<-sample(N,N,replace=T)
    mn.b<-c(mn.b,min(apply(E[ib,],2,mean)))
  }

  mn-mean(mn.b)


}




pval.class<-function(Y.hat,Y.tr,Y.val,r=1){
  # Y.hat prediction
  # Y.tr output training set
  #Y.val output validation set

  if (!(is.factor(Y.hat) & is.factor(Y.tr) & is.factor(Y.val) ))
  {browser()
    stop("All the inputs should be factors")

  }
  L<-levels(Y.tr)

  pi.tr<-numeric(length(L))

  pi.ts<-numeric(length(L))
  for (i in 1:length(L)){
    pi.tr[i]<-length(which(Y.tr==L[i]))/length(Y.tr)
    pi.ts[i]<-length(which(Y.val==L[i]))/length(Y.val)

  }

  th<-0
  for (i in 1:length(L))
    th<-th+(1-pi.tr[i])*pi.ts[i]

  q<-length(which(Y.val!=Y.hat))/length(Y.val)


  1-(1-pnorm((length(Y.val)*(q-th)+0.5)/(sqrt(length(Y.val)*th*(1-th)))))^r


}
pval.class2<-function(e,Y.tr,Y.val,r=1){
  # Y.hat prediction
  # Y.tr output training set
  #Y.val output validation set

  if (!( is.factor(Y.tr) & is.factor(Y.val) ))
    stop("All the inputs should be factors")
  L<-levels(Y.tr)

  pi.tr<-numeric(length(L))

  pi.ts<-numeric(length(L))
  for (i in 1:length(L)){
    pi.tr[i]<-length(which(Y.tr==L[i]))/length(Y.tr)
    pi.ts[i]<-length(which(Y.val==L[i]))/length(Y.val)

  }

  th<-0
  for (i in 1:length(L))
    th<-th+(1-pi.tr[i])*pi.ts[i]

  q<-length(which(e==1))/length(e)


  pn<-1-(1-pnorm((length(Y.val)*(q-th)+0.5)/(sqrt(length(Y.val)*th*(1-th)))))^r

  pb<-1-(1-pbinom(length(Y.val)*q,length(Y.val),th))^r

  pb


}

stat.class2<-function(e,Y.tr,Y.val){
  # Y.hat prediction
  # Y.tr output training set
  #Y.val output validation set

  if (!( is.factor(Y.tr) & is.factor(Y.val) ))
    stop("All the inputs should be factors")
  L<-levels(Y.tr)

  pi.tr<-numeric(length(L))

  pi.ts<-numeric(length(L))
  for (i in 1:length(L)){
    pi.tr[i]<-length(which(Y.tr==L[i]))/length(Y.tr)
    pi.ts[i]<-length(which(Y.val==L[i]))/length(Y.val)

  }

  th<-0
  for (i in 1:length(L))
    th<-th+(1-pi.tr[i])*pi.ts[i]

  q<-length(which(e==1))/length(e)


  (length(Y.val)*(q-th)+0.5)/(sqrt(length(Y.val)*th*(1-th)))


}

#' cor2I2
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description correlation to mutual information under hypothesis of normality
#' @details correlation to mutual information under hypothesis of normality
#' @title correlation to mutual information
#' @name cor2I2
#' @export
#'
#' @param  rho: Pearson correlation
#' @return mutual information
#' @export
#' @examples
#' cor2I2(-0.9)
#'
cor2I2<-function(rho){
  rho<-pmin(rho,1-1e-5)
  rho<-pmax(rho,-1+1e-5)
  -1/2*log(1-rho^2)
}

#' var2H
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description variance to entropy under hypothesis of normality
#' @details variance to entropy under hypothesis of normality
#' @title variance to entropy
#' @name var2H
#' @export
#'
#' @param  v: variance
#' @return entropy
#' @export
#' @examples
#' var2H(-0.9)
#'
var2H<-function(v){
  1/2*(1+log(2*pi*v))
}



corDC<-function(X,Y){
  ## correlation continuous matrix and discrete vector
  ## NB: the notion of sign has no meaning in this case. Mean of absolute values is taken
  ## 14/11/2011

  if (!is.factor(Y))
    stop("This is not the right function. Y is not a factor !!")

  N<-NROW(X)
  L<-levels(Y)

  if( length(L)==2)
    lL<-1
  else
    lL<-length(L)

  cxy<-NULL
  for (i in 1:lL){
    yy<-numeric(N)
    ind1<-which(Y==L[i])
    ind2<-setdiff(1:N,ind1)
    yy[ind1]<-1
    cxy<-cbind(cxy,abs(cor(X,yy)))
  }

  apply(cxy,1,mean)
}

corXY<-function(X,Y){
  ## correlation continuous matrix and continuous/discrete vectormatrix
  ## 14/11/2011

  n<-NCOL(X)
  N<-NROW(X)
  m<-NCOL(Y)

  cXY<-array(NA,c(n,m))

  for (i in 1:m){
    if (m==1)
      YY<-Y
    else
      YY<-Y[,i]
    if (is.numeric(YY)){
      cXY[,i]<-cor(X,YY,use="pairwise.complete.obs")
    } else {
      cXY[,i]<-corDC(X,YY)
    }
  }
  cXY
}
corXY.old<-function(X,Y){
  ## correlation computed by linear regression
  ## 30/1/2012

  n<-NCOL(X)
  N<-NROW(X)
  ##  m<-NCOL(Y)

  cXY<-numeric(n)


  for (i in 1:n)
    cXY[i]<-abs(regrlin(X[,i],Y)$beta.hat[2])


  cXY<-pmin(1,(cXY)/(2*median(cXY)))
  cXY
}


#' pcor1
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references Handbook \emph{Statistical foundations of machine learning} available in \url{http://www.ulb.ac.be/di/map/gbonte/mod_stoch/syl.pdf}
#' @description partial correlation cor(x,y|z)
#' @details partial correlation cor(x,y|z)
#' @title partial correlation
#' @name pcor1
#' @export
#'
#' @param  x: vector
#' @param  y: vector
#' @param  z: vector
#' @return partial correlation
#' @export
#' @examples
#' pcor1(rnorm(100),rnorm(100),rnorm(100))
#'
pcor1<-function(x,y,z){
  if (is.numeric(z)){
    rho.xy<-cor(x,y,"pairwise.complete.obs")
    rho.xz<-cor(x,z,"pairwise.complete.obs")
    rho.yz<-cor(z,y,"pairwise.complete.obs")
    if (is.na(rho.xz+rho.yz+rho.xy))
      return(0)
    if (rho.xz==1 | rho.yz==1)
      return(0)
    rho<-(rho.xy-rho.xz*rho.yz)/(sqrt(1-min(rho.xz^2,0.99))*sqrt(1-min(rho.yz^2,0.99)))
    return(rho)
  } else {
    stop("z should be numeric")

  }

}


Icond<-function(x,y=NULL,z,lambda=0){
  ## conditional  information cor(x,y|z)
  ## 14/10/11
  ## added x=matrix case on 25/10/11

  ## numeric z
  if (is.numeric(z)){
    if (is.vector(x))
      return(cor2I2(pcor1(x,y,z)))
    X<-x
    n<-NCOL(X)
    Ic<-array(0,c(n,n))
    for (i in 1:(n-1))
      for (j in (i+1):n){
        Ic[i,j]<-Icond(X[,i],X[,j],z)
        Ic[j,i]<-Ic[i,j]
      }
    return(Ic)

  }
  ## factor z and vectors x and y
  if (! is.null(y)){
    L<-levels(z)
    lL<-length(L)
    w<-numeric(lL)
    for (i in 1:lL)
      w[i]<-length(which(z==L[i]))
    w<-w/sum(w)

    Ic<-NULL
    for (i in 1:lL){
      ind1<-which(z==L[i])
      Ic<-c(Ic,cor2I2(cor(x[ind1],y[ind1])))
    }

    return(as.numeric(w*Ic))
  }

  require(corpcor)
  ## factor z and matrix x
  X<-x
  n<-NCOL(X)
  L<-levels(z)
  lL<-length(L)
  w<-numeric(lL)
  for (i in 1:lL)
    w[i]<-length(which(z==L[i]))
  w<-w/sum(w)

  Ic<-array(0,c(n,n))
  W<-0
  for (i in 1:lL){
    ind1<-which(z==L[i])


    if (length(ind1)>8){


      Ic<-Ic+w[i]*cor2I2(cor.shrink(X[ind1,],lambda=lambda,v=F))
      W<-W+w[i]
    }
  }


  return(Ic/W)

}


ppears<-function(r.hat,N,S=0){
  n<-length(r.hat)
  p<-numeric(n)

  for (i in 1:n){
    z<-abs(0.5*(log(1+r.hat[i])-log(1-r.hat[i])))*sqrt(N[i]-S-3)

    p[i]<-pnorm(z,lower.tail=F)


  }
  p
}
rpears<-function(r,N,S=0){
  ## random generator of R samples according to the sampling distribution
  ## of the Pearson correlation with N samples and S conditional variables
  nr<-NROW(r)
  cr<-NCOL(r)
  r<-c(r)
  r<-pmax(-1+1e-3,pmin(1-1e-3,r))
  S<-0

  z<-0.5*(log(1+r)-log(1-r))
  sd.z<-1/sqrt(N-S-3)
  no<-rnorm(length(r),sd=sd.z)
  D<-z+no

  rhoD<-(exp(2*D)-1)/(exp(2*D)+1)
  rhoD<-array(rhoD,c(nr,cr))

}

nl.minf<-function(x,y, R=10){
  ## nonlinear mutual information
  N<-length(x)
  V=var(y)
  is<-sort(x,decr=FALSE,index=TRUE)$ix
  x<-x[is]
  y<-y[is]

  R=min(R,N)
  D=max(5,round(N/R))
  m<-NULL
  v<-NULL
  for (r in 1:R){
    ir<-max(1,((r-1)*D)):min(N,r*D-1)

    m<-c(m,mean(y[ir]))  ## E[y|x]
    v<-c(v,var(y[ir]))  ## V[y|x]

  }
  ## V[y]=E_x[V[y|x]]+V_x[E[y[x]]] => E_x[V[y|x]] = V[y] - V_x[E[y[x]]]
  Vc=mean(c(mean(v,na.rm=TRUE),max(0,V-var(m,na.rm=TRUE))))

  return(var2H(V)-var2H(Vc))
}

normv<-function(x,p=2){
  sum(x^p)^(1/p)

}
