rm(list=ls())
library(reticulate)
library(tree)
## classification example
n=20
N=200
X=array(rnorm(N*n),c(N,n))
Y=numeric(N)
ppy=X[,1]*X[,2]*X[,3]+X[,n]+log(abs(X[,3]))+abs(X[,n-1])+rnorm(N,sd=0.2)
Y[which(ppy>median(ppy))]<-1
Y=factor(Y)
Itr=sample(N,round(N/2))
Its=setdiff(1:N,Itr)
Xtr=X[Itr,]
Ytr=Y[Itr]
Xts=X[Its,]
Yts=Y[Its]

algos=c("boost","qda","lda","logistic","svm","nb","lazy","py.sgd_class",
        "py.rf_class","py.gb_class","py.piperf_class","py.gp_class","py.nb_class",
        "py.ab_class","py.knn_class","py.lsvm_class")
MISCL=NULL
aAUC=NULL
for (a in 1:length(algos)){
  Yhat=pred(algos[a],Xtr,Ytr,Xts, classi=TRUE)
  e=as.numeric(Yts!=Yhat$pred)
  ## misclassification error
  MISCL=c(MISCL,sum(e)/length(Its))
  aAUC=c(aAUC,AUC(Yts,Yhat$prob[,2]))
 
}
names(MISCL)=algos

print(sort(MISCL,decr=FALSE))
cat("\n -----\n")

names(aAUC)=algos

print(sort(aAUC,decr=TRUE))