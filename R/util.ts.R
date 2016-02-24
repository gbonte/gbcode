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
  mean(abs(ts1-ts2)/(ts1+ts2)/2)*100

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
#' @param hor [no.var]: vector of predicted horizons (hor=1 boils down to one-step-ahed prediction)
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

list(inp=Input,out=Output)
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
