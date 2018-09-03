library(gbcode)
library(xgboost)

N=100
n=10
X<-array(rnorm(N*n),c(N,n))
Y<-X[,1]*X[,n]+array(rnorm(N,sd=0.1),c(N,1))


#bst <- xgboost(data =X, label = Y,nrounds=10)
#predict(bst,X)
Yhat=pred("gbm",X,Y,X,classi=FALSE,ntrees=1000,distribution="gaussian")

e=Y-Yhat
print(mean(e^2))