rm(list=ls())
library(nnet)


f<-function(X,sd=0){
  y<-X[,1]*X[,2]+X[,2]+sd*rnorm(NROW(X))
  y
}


set.seed(0)
sdw=0.25
N=150

X<-array(rnorm(2*N,sd=0.5),c(N,2))
Y<-f(X,sdw)

S<-c(1,2,3,5,7,8,9,10)

MISEhat<-numeric(max(S))+Inf
df<-data.frame(cbind(Y,X))
colnames(df)[1]<-'Y'

initrang=0.00
for (s in S){
  ## structural identification loop
  Eloo<-NULL
  for (j in 1:N){
    ## leave-one-out loop
    h<- nnet(Y~.,df[-j,],size=s,rang=initrang,
                    linout=TRUE,trace=FALSE)
    Eloo<-c(Eloo,Y[j]- predict(h,df[j,]) )
    
  }
  MISEhat[s]=mean(Eloo^2)
}
browser()
stilde=which.min(MISEhat)
h<- nnet(Y~.,df,size=stilde,rang=initrang,
         linout=TRUE,trace=FALSE)

