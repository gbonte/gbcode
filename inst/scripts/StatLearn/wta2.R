rm(list=ls())
library(nnet)
set.seed(0)

f<-function(X,sd=0.25){
  y<-X[,1]*X[,2]+X[,2]+sd*rnorm(NROW(X))
  y
}


## training set generation
Ntr=150
Xtr<-array(rnorm(2*Ntr,sd=0.5),c(Ntr,2))
Ytr<-f(Xtr,sdw)

## test set generation
Nts=200000
Xts<-array(rnorm(2*Nts,sd=0.5),c(Nts,2))
Yts<-f(Xts,sdw)

## set of hyperparameters
S<-c(1,2,3,5,7,8,9,10)

MSEhat<-numeric(max(S))+Inf
df<-data.frame(cbind(Ytr,Xtr))
colnames(df)[1]<-'Y'
dfts<-data.frame(cbind(Yts,Xts))
colnames(dfts)[1]<-'Y'
initrang=0.001


for (s in S){
  ## structural loop
  Eloo<-NULL
  for (i in 1:Ntr){
    ## leave-one-out loop
    
    h<- nnet(Y~.,df[-i,],size=s,rang=initrang,
                    linout=TRUE,trace=FALSE)
    ## parametric identification
    
    Eloo<-c(Eloo,Ytr[i]- predict(h,df[i,]) )
  }
  MSEhat[s]=mean(Eloo^2)
  cat(".")
}

## assessment on in
MSEtest<-numeric(max(S))+Inf
for (s in S){
  h<- nnet(Y~.,df, size=s,rang=initrang,
           linout=TRUE,trace=FALSE)
  Ets<-Yts- predict(h,dfts) 
  MSEtest[s]=mean(Ets^2)
}

cat("\n", which.min(MSEhat), ":",which.min(MSEtest),"\n")
plot(S,MSEhat[S],type="l",xlab="# hidden neurons",ylim=c(0,0.15),ylab="MISE")
lines(S,MSEtest[S],col="green")
legend("topright",c("loo","test"),col=c("black","green"),lty=1)