# Calculate activation function
rm(list=ls())
library(nnet)
sigmoid <- function(z){1.0/(1.0+exp(-z))}

# Partial derivative of activation function
sigmoid_prime <- function(z){sigmoid(z)*(1-sigmoid(z))}


### W= w01.1, w11.1, w02.1, w12.1, w03.1, w13.1,  w11.2, w21.2,w31.2, B

nnetf<-function(x,W){
  
  a1<-W[2]*x+ W[1]
  z1<-sigmoid(a1)
  a2<-W[4]*x+ W[3]
  z2<-sigmoid(a2)
  a3<-W[6]*x+ W[5]
  z3<-sigmoid(a3)
  
  y=W[7]*z1+W[8]*z2+W[9]*z3 +W[10]
  y
  
}

### W= w01.1, w11.1, w02.1, w12.1,  w11.2, w21.2, B
gradnet<-function(X,E,W){
  
  G<-numeric(length(W))
  N<-length(X)
  for (i in 1:N){
    x=X[i]
    e=E[i]
    a1<-W[2]*x+ W[1]
    z1<-sigmoid(a1)
    a2<-W[4]*x+ W[3]
    z2<-sigmoid(a2)
    
    a3<-W[6]*x+ W[5]
    z3<-sigmoid(a3)
    
    
    gw01.1=W[7]*sigmoid_prime(a1)
    gw11.1=W[7]*sigmoid_prime(a1)*x
    
    gw02.1=W[8]*sigmoid_prime(a2)
    gw12.1=W[8]*sigmoid_prime(a2)*x
    
    gw03.1=W[9]*sigmoid_prime(a3)
    gw13.1=W[9]*sigmoid_prime(a3)*x
    
    
    gw11.2=z1
    gw21.2=z2
    gw31.2=z3
    
    gB=1
    
    G<-G-2*e*c(gw01.1,gw11.1,gw02.1,gw12.1,gw03.1, gw13.1 ,
               gw11.2, gw21.2 , gw31.2, gB)
  }
  
  G/N
}


N<-100
X=sort(runif(N,-1,1))
Y=sin(pi*X)+rnorm(N,sd=0.25)


d<-data.frame(Y,X)
names(d)<-c("Y","X")



mod.nn<-nnet(Y~.,data=d,size=3,skip=FALSE,
             trace=T, maxit=3000,linout=TRUE,rang=0.2)

p<-predict(mod.nn,d$X)
E=Y-p
print(mean(E^2))
plot(X,Y)
lines(X,p)
browser()
minE=Inf
for (it in 1:200){
  W<-rnorm(10,sd=2)
  Yhat=nnetf(X,W)
  E=mean((Y-Yhat)^2)
  
  if (E< minE){
    minE=E
    W0=W
  }
  
}

W=W0
eta=0.1
for (it in 1:50000){
  
  Yhat=nnetf(X,W)
  E=Y-Yhat
  wx=1:N
  W=W-eta*gradnet(X[wx],E[wx],W)
  if (it %%500==0){
    print(mean(E^2))
    plot(X,Y)
    lines(X,Yhat)
    browser() 
  }
  
}

plot(X,Y)
lines(X,Yhat)
