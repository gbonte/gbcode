
smape<-function(ts1,ts2,Cent,Sc){
  ts1<-Cent+ts1*Sc
  ts2<-Cent+ts2*Sc
  mean(abs(ts1-ts2)/(ts1+ts2)/2)*100

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

remNA<-function(TS){

  return(approx(seq(TS),TS,seq(TS))$y)

}


MakeInput<-function(ts, n, delay,hor=1,w=1){

no.data<-NROW(ts)
no.var<-NCOL(ts)
a<-NROW(n)
b<-NCOL(n)
if (a!=no.var)
	stop('Error in the size of n')


N<-no.data-max(n)-max(delay)

Input<-array(0,c(N,sum(n)))
Output<-array(0,c(N,hor))

for (i in 1:N) {
  for (j in 1:no.var) {
##    for (k in 1:n[j]) {
##      Input[i,sum(n[1:j-1])+k]<-ts[i+n[j]-k+max(n)-n[j]+max(delay)-delay[j],j]

##    }
    k<-1:n[j]
    Input[i,sum(n[1:j-1])+k]<-ts[i+n[j]-k+max(n)-n[j]+max(delay)-delay[j],j]

    Output[i,1:hor]<-numeric(hor)+NA
    M<-min(no.data,(i+max(n+delay)+hor-1))

    Output[i,1:(M-(i+max(n+delay))+1)]<-ts[(i+max(n+delay)):M,w]
  }

}

list(inp=Input,out=Output)
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
