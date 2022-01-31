## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())
source("kNN.R")
set.seed(0)


KNN.wrap<-function(X,Y,size,K=1){
## leave-one-out wrapper based on forward selection and KNN
  n<-ncol(X)
  N<-nrow(X)
  selected<-NULL
  while (length(selected)<size){
    miscl.tt<-numeric(n)+Inf
    for (j in 1:n){
      if (! is.element(j,selected)){ 
        select.temp<-c(selected,j)
        miscl<-numeric(N)
        for (i in 1:N) {
          X.tr<-array(X[-i,select.temp],c(N-1,length(select.temp)))
          Y.tr<-Y[-i]
          q<-array(X[i,select.temp],c(1,length(select.temp)))
          Y.ts<-Y[i]
          Y.hat.ts <- KNN(X.tr, Y.tr,K,q)
          miscl[i]<-Y.hat.ts!=Y.ts          
        }
        miscl.tt[j]<-mean(miscl)             
      }
    }
    selected<-c(selected,which.min(miscl.tt))
    cat(".")
    
  }
  cat("\n")
  selected
}



K=3 ## number of neighbours in KNN
data(golub)  ## dataset upload

X<-golub$X
N<-nrow(X)
Y=factor(golub$Y)


I<-sample(N)
X<-scale(X)


X<-X[I,]
Y<-Y[I]

## Training/test partition
N.tr<-40
X.tr<-X[1:N.tr,]
Y.tr<-Y[1:N.tr]
N.ts<-32
X.ts <- X[(N.tr+1):N,]
Y.ts<-Y[(N.tr+1):N]


## preliminary dimensionality reduction by ranking
ind.filter<-rankrho(X.tr,Y.tr,100)
X.tr=X.tr[,ind.filter]
X.ts=X.ts[,ind.filter]


## wrapper feature selection 
wrap.var<-KNN.wrap(X.tr,Y.tr,size=20,K)


###########################################
# Assessement of classification in the testset

for ( size in c(2:length(wrap.var))){
  
  miscl<-numeric(N.ts)
  Y.hat.ts<-numeric(N.ts)
  Conf.tt<-array(NA,c(2,2))
  for (i in 1:N.ts){
    q<-X.ts[i,]
    Y.hat.ts[i]<-KNN(X.tr[,wrap.var[1:size]],Y.tr,K,q[wrap.var[1:size]])
    miscl[i]<-Y.hat.ts[i]!=Y.ts[i]
  }
  
  miscl.tt<-mean(miscl)
  rownames(Conf.tt)=c("pred=0","pred=1")
  colnames(Conf.tt)=c("real=0","real=1")
  Conf.tt[1,1]<-length(which(Y.hat.ts=="0" & Y.ts =="0"))
  Conf.tt[1,2]<-length(which(Y.hat.ts=="0" & Y.ts =="1"))
  Conf.tt[2,1]<-length(which(Y.hat.ts=="1" & Y.ts =="0"))
  Conf.tt[2,2]<-length(which(Y.hat.ts=="1" & Y.ts =="1"))
  
  
  print(paste("K=",K, "size=",size,"; Misclass %=",miscl.tt))
  print(Conf.tt)
  
}


