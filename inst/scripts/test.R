library(gbcode)
library(xgboost)

N=100
n=10
X<-array(rnorm(N*n),c(N,n))
Y<-X[,1]+array(rnorm(N,sd=0.1),c(N,1))


#bst <- xgboost(data =X, label = Y,nrounds=10)
#predict(bst,X)
Yhat=pred("xgboost",X,Y,X,classi=FALSE,nrounds=10)

e=Y-Yhat
print(mean(e^2))