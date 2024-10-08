% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mstep.R
\name{MmultiplestepAhead}
\alias{MmultiplestepAhead}
\title{MmultiplestepAhead}
\arguments{
\item{TS:}{time series [T,m] where m>1 is the number of series}

\item{n:}{embedding order}

\item{dfmlmethods:}{alternative methods from \link{MmultiplestepAhead} used by DFML to predict factors}

\item{H:}{horizon}

\item{unimethod:}{method from \link{MmultiplestepAhead} used to predict each univariate series}

\item{multi:}{\itemize{
\item{UNI}: prediction based on univariate forecasting with unimethod in \link{MmultiplestepAhead}
\item{DFM}: prediction based on DFM
\item{DFML}: prediction based on DFML
\item{VAR}: prediction based on VAR 
\item{VARs}: prediction based on VAR shrink from \pkg{VARshrink} package
\item{RNN}: prediction based on python (\pkg{reticulate}) implementation of rnn (recurrent neural network)
\item{LSTM}: prediction based on python (\pkg{reticulate}) implementation of  lstm (long short term memory)
\item{TRANSF}: prediction based on python (\pkg{reticulate}) implementation of a transformer
\item{MIMO_rr}: prediction based on a MIMO ridge regressor (lambda estimation based on PRESS)
\item{MITER_rr}: prediction based on an iterated ridge regressor (lambda estimation based on PRESS and a criterion with horizon Hobj) 
\item{MIMO_red}: prediction based on a MIMO reduced rank regressor (\pkg{rrpack})
\item{MIMO_cca}: prediction based on a MIMO CCA 
\item{MIMO_pls}: prediction based on a MIMO partial least-squares (Sklearn python)
\item{MIMO_las}: prediction based on a MIMO lasso (Sklearn python)
\item{MIMO_rf}: prediction based on a MIMO Random Forest (Sklearn python)
\item{MIMO_ml}: prediction based on MIMO direct strategy and predictor from \link{pred}
\item{MIMO_fs}: prediction based on MISO direct strategy and feature selection (predictor from \link{pred})
}}
}
\value{
matrix [H,m] with the H step ahead predictions for m series
}
\description{
MmultiplestepAhead: multi-variate multi-step-ahead forecaster
}
\details{
MmultiplestepAhead

Wrapper over a set of methods for multi variate multiple step ahead time series prediction


The python forecasters require the installation of \pkg{reticulate} and several python packages (scikit-learn, tensorflow, keras)
}
\examples{
## Multi-variate Multi-step-ahead time series forecasting
rm(list=ls())
library(gbcode)
library(reticulate)
set.seed(0)
N=1000
m=3
n=3 
H=10
TS<-array(0,c(N,m))
for (j in 1:m){
 for (f in 1:5)
   TS[,j]=TS[,j]+sin(2*pi*(1:(N))/runif(1,8,20))
 TS[,j]=TS[,j]+rnorm(N,sd=0.3)

}
TS=scale(TS)
N=NROW(TS)

#P=MmultiplestepAhead(TS[1:(N-H),],n=n,H=H,multi="RNN",
#                    nepochs=100, nunits=10)
P=MmultiplestepAhead(TS[1:(N-H),],n=n,H=H,multi="MITER_rr",
                    nLambdas=150)
if (m==1)
 P=cbind(P)
cat("MSE=",mean((P-TS[(N-H+1):N,])^2),"\n")
par(mfrow=c(1,m))
Nvis=round(N-4*H)
for (j in 1:m){
 Yhat=numeric(N)+NA
 Yhat[(N-H+1):N]=P[,j]
 plot(TS[Nvis:N,j],type="l",
      main=paste("MSE=",round(mean((TS[(N-H+1):N,j]- Yhat[(N-H+1):N])^2),2)))
 lines(Yhat[Nvis:N],col="red",lw=3)
}

}
\references{
\url{https://tinyurl.com/sfmlh}
}
\author{
Gianluca Bontempi  \email{Gianluca.Bontempi@ulb.be}
}
