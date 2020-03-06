library(gbcode)
library(xgboost)
set.seed(0)
N=100
n=20
X<-array(rnorm(N*n),c(N,n))
Y<-(X[,1]*X[,n]+X[,2]*abs(X[,3]*X[,4])+array(rnorm(N,sd=0.01),c(N,1)))
Y<-as.numeric(Y<median(Y))

Xts<-array(rnorm(N*n),c(N,n))
Yts<-round(Xts[,1]*Xts[,n]+Xts[,2]*abs(Xts[,3]*Xts[,4])+array(rnorm(N,sd=0.01),c(N,1)))
Yts<-as.numeric(Yts<median(Y))
#bst <- xgboost(data =X, label = Y,nround=100)
#Yhat<-predict(bst,Xts)
#Yhat=pred("gbm",X,Y,Xts,classi=FALSE,ntrees=1000,distribution="gaussian")
#Yhat=pred("rf",X,factor(Y),Xts,classi=TRUE,ntrees=1000)$pred
#e=length(which(Yts!=(Yhat)))/length(Yts)
#print(e)

dtrain <- xgb.DMatrix(X, label = Y)

cv <- xgb.cv(data = dtrain, nrounds = 3, nthread = 2, nfold = 5, 
             metrics = list("auc"),
             max_depth = 3, eta = 1, showsd = T,objective = "binary:logistic")
p
Yhat=pred("xgboost",X,Y,Xts,classi=FALSE,nrounds=1, max_depth=2,eta=0.3)
Yhat2=Yhat
e2=length(which(Yts!=round(Yhat2)))/length(Yts)
e=e2
print(e2)