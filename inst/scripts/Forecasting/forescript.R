rm(list=ls())
library(reticulate)
library(gbcode)

library(keras)
set.seed(0)
N=200
m=3
H=10
n=24
TS<-array(0,c(N,m))
for (j in 1:m){
  for (f in 1:15)
    TS[,j]=TS[,j]+rnorm(1)*sin(2*pi*(1:(N))/runif(1,2,20))
  TS[,j]=TS[,j]+rnorm(N,sd=0.3)
}
TS=scale(TS)
N=NROW(TS)

if (m>1){
  #P=MmultiplestepAhead(TS[1:(N-H),],n=n,H=H,multi="RNN",
  #                    nepochs=100, nunits=10)
  #P=MmultiplestepAhead(TS[1:(N-H),],n=n,H=H,multi="MIMO_rr",
  #                     nLambdas=50)
  P=MmultiplestepAhead(TS[1:(N-H),],n=n,H=H,multi="TRANSF",epochs=1000)
} else {
  ## P=multiplestepAhead(TS[1:(N-H),],n=n,H=H,method="mimo_rr",epochs=1000)
  P=multiplestepAhead(TS[1:(N-H),],n=n,H=H,method="transf",epochs=1000)
  
}



if (m==1)
  P=cbind(P)

cat("MSE=",mean((P-TS[(N-H+1):N,])^2),"\n")
par(mfrow=c(1,m))
Nvis=round(N-5*H)
for (j in 1:m){
  Yhat=numeric(N)+NA
  
  Yhat[(N-H+1):N]=P[,j]
  plot(TS[Nvis:N,j],type="l",
       main=paste("MSE=",round(mean((TS[(N-H+1):N,j]- Yhat[(N-H+1):N])^2),2)))
  lines(Yhat[Nvis:N],col="red",lw=3)
}

