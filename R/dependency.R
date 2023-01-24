#' @import mvtnorm MASS




Posdef <- function (n, ev = runif(n, 0, 10)) {
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp)
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}

RKfct<-function(X,NK=3){
  ## it returns the output of a function sum_{i=1}^NK alpha_i K(xi,x) 
  ## belonging to the RKHS space defined by
  ## a kernel K(,), NK random basis and NK random coefficients alpha_i
  require(kernlab)
  ke <- rbfdot(sigma =runif(1,0.01,0.1))
  ## calculate kernel matrix
  n<-NCOL(X)
  N=NROW(X)
  basis=array(NA,c(NK,n))
  for (j in 1:n)
    basis[,j]=X[sample(1:N,NK),j]
  ## basis built by taking random points from X
  K=kernelMatrix(ke, basis)
  Kts=kernelMatrix(ke, X,basis)
  alpha=array(rnorm(NK),c(NK,1)) ## NK random coefficients
  Yhat=scale(Kts%*%alpha)  
  return(Yhat)
}

nonlinfct<-function(X,f=1){
  ## multivariate regression benchmark
  n<-NCOL(X)
  N<-NROW(X)
  # it uses a set of nonlinear bivariate functions g(x,y):
  ## it first creates the vectors x and y (dependeing on different portions of the inputs)
  # and then it computes g(x,y)
  fx<-sample(n,sample(2:(n-2),1))
  fy<-setdiff(1:n,fx)
  #cat(fx,":",fy,";",n,"\n")
  L=length(fx)
  if (L==1){
    x=abs(X[,fx])
  } else {
    x=apply(abs(X[,fx]),1,mean)
  }
  if (length(fy)==1){
    y=abs(X[,fy])
  } else {
    y=apply(abs(X[,fy]),1,median)
  }
  switch(f,
         {Yhat=log(x^2+y^2+0.01)},  #1 
         {Yhat=sqrt(abs(sin(x^2+y^2)))},
         {Yhat=log(x*y^2+x^2*y+0.01)},
         {Yhat=sqrt(abs(x^2/(y+1)))},
         {Yhat=1/(x^2+y^2+1)},
         {Yhat=(x*sin(x*y))/(x^2+y^2+1)},
         {Yhat=y*exp(2*sqrt(abs(x)))},
         {Yhat=y*sin(x)+x*sin(y)},
         {Yhat=(x^3-2*x*y+y^2)/(x^2+y^2+1)},
         {Yhat=x+y+log(abs(x*y)+0.01)}, #10 
         {Yhat=sin(x)+log(y+0.01)},
         {Yhat=sin(pi*x*y)},
         {Yhat=RKfct(X,sample(5:round(2*N/3),1))},
         {Yhat=RKfct(X,sample(10:round(2*N/3),1))},
         {Yhat=RKfct(X,sample(15:round(2*N/3),1))},
         {Yhat=RKfct(X,sample(20:round(2*N/3),1))},
         {Yhat=RKfct(X,sample(25:round(2*N/3),1))},
         {Yhat=RKfct(X,sample(30:round(2*N/3),1))},
         {Yhat=RKfct(X,sample(35:round(2*N/3),1))},
         {Yhat=RKfct(X,sample(40:round(2*N/3),1))})  #20
  if (any(is.na(Yhat)))
    browser()
  return(Yhat)
}




#### regrDataset ####
#' Generator of nonlinear regression dataset
#'
#' @author Gianluca Bontempi  \email{gbonte@@ulb.ac.be}
#' @references \url{https://tinyurl.com/sfmlh}
#' @title Generator of nonlinear regression dataset
#'@export
#'
#'@param N: number of observations
#'@param n: number of observed variables
#'@param neff: number of effective features
#'@param sdn: standard deviation of Gaussian noise
#'@param seed: seed random generator
#'
#'@examples
#'
#'N<-100
#'n<-5
#'neff<-3
#'R<-regrDataset(N,n,neff,0.1)
#'X<-R$X
#'Y<-R$Y
#'feat<-R$feat
#'@name regrDataset
#'
#'
#'
regrDataset<-function(N,n,neff,sdn,seed=0,diag=TRUE){
  set.seed(seed)
  n=max(n,4)
  neff=max(neff,3)
  Sigma=diag(n)
  if (!diag)
    Sigma=Posdef(n)
  X<-scale(rmvnorm(N,sigma=Sigma ))
  feat<-sample(n,neff)
  XX=X[,feat]
  f<-sample(20,1)
  Y<-nonlinfct(XX,f)+rnorm(N,sd=sdn)
  list(X=X,Y=Y,feat=feat,f=f)
}


