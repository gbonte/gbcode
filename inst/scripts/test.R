library(gbcode)

N=100
n=10
X<-array(rnorm(N*n),c(N,n))
Y<-X[,1]+array(rnorm(N,sd=0.1),c(N,1))

Yhat=pred("lasso",X,Y,X,classi=FALSE)

e=Y-Yhat
print(mean(e^2))