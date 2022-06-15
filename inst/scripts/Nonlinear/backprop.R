
rm(list=ls())

g1<-function(a){
  1/(1+exp(-a))
}
gprime1<-function(a){
  exp(-a)/((1+exp(-a))^2)
}

g2<-function(a){
  a
}
gprime2<-function(a){
  a*0+1
}


N=1000
n=1
X=cbind(runif(N,-1,1))
Y=scale(X[,1]^3+rnorm(N,sd=0.1))
plot(X,Y)
H1=2
Z0=array(X,c(n,N))
H2= 1 ## single output
W1=array(rnorm(N*H1,sd=0.3),c(n,H1))
W2=array(rnorm(N*H1,sd=0.3),c(H1,H2)) ## [H1,H2]
eta=0.1

J=NULL
for (rep in 1:10000){
  a1=t(W1)%*%Z0   ## [H1,H0]*[H0,N]=[H1,n]*[n,N]
  Z1=g1(a1)  ## [H1,N]
  G1=gprime1(a1)
  
  a2=t(W2)%*%Z1 ## [H2,N]
  Z2=g2(a2)
  Yhat=t(Z2) ## [N,1]
  
  e=(Y-Yhat)
  J=c(J,mean(e^2)/var(Y))
  
  
  
  D2=gprime2(a2)
  D1=(W2%*%D2)*G1
  
  derW2=0
  derW1=0
  
  for (i in 1:N){
    derW2=derW2+e[i]*(Z1[,i]%*%t(D2[,i]))
    derW1=derW1+e[i]*(Z0[,i]%*%t(D1[,i]))
  }
  
  W1=W1+eta*derW1/N
  W2=W2+eta*derW2/N
  
  if (rep %%200==0){
    par(mfrow=c(1,2))
    plot(X,Y)
    points(X,Yhat,col="red")
    plot(J,ylab="NMSE")
    browser()
  }
}

plot(J)
plot(X,Y)
points(X,Yhat,col="red")