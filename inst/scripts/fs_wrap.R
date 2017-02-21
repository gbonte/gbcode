##INFO-F-528 Machine learning methods for bioinformatics
## Exercise session 6

rm(list=ls())
source("kNN.R")
n.max<-40
load("golub.Rdata")
load("golub.filter.Rout")
source("KNNforward.R")
KNN.wrap<-function(X,Y,size,K=1){

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
    print(selected)
    
  }
  selected
}


N<-nrow(X)
set.seed(0)
I<-sample(N)
X<-X[I,ind.filter3]
Y<-Y[I]


N.tr<-40
X.tr<-X[1:N.tr,]
Y.tr<-Y[1:N.tr]

wrap.var<-KNNforward(X.tr,Y.tr,nmax=10)

N.ts<-32
X.ts <- X[(N.tr+1):N,]
Y.ts<-Y[(N.tr+1):N]

K=1
###########################################
# Training-and-test

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
  Conf.tt[1,1]<-length(which(Y.hat.ts=="ALL" & Y.ts =="ALL"))
  Conf.tt[1,2]<-length(which(Y.hat.ts=="ALL" & Y.ts =="AML"))
  Conf.tt[2,1]<-length(which(Y.hat.ts=="AML" & Y.ts =="ALL"))
  Conf.tt[2,2]<-length(which(Y.hat.ts=="AML" & Y.ts =="AML"))
  
  
  print(paste("K=",K, "size=",size,"; Miscl tt=",miscl.tt))
  
  
}


