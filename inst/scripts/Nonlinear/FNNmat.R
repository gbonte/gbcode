## Matrix based implementation of the FNN backpropagation algorithm

rm(list=ls())
set.seed(1)
n=5 
## size input

g<-function(X){
  ## sigmoid activation function
  1/(1+exp(-X))
}

gprime<-function(X){
  ## derivative sigmoid activation function
  exp(-X)/((1+exp(-X))^2)
}


g2<-function(X){
  ## linear activation function
  X
}

gprime2<-function(X){
  ## derivative linear activation function
  array(1,dim(X))
}




L=5
## number of layers
H=numeric(L)+1
H[1:(L-1)]=round(seq(10,2,length=L-1))
## hidden nodes per layer

## initializazion weights
W<-NULL
for (l in 1:L){
  if (l==1)
    W<-c(W,list(array(rnorm((n)*(H[l]),sd=1),c(n,(H[l])))))
  else
    W<-c(W,list(array(rnorm((H[l-1])*(H[l]),sd=1),c(H[l-1],H[l]))))
}

## W[[l]] matrix of size (H[l-1],H[l])




N=150 ## training sample
Xtr=array(rnorm(N*(n),sd=0.5),c(N,n))
Ytr=Xtr[,1]+Xtr[,2]^2-Xtr[,n-1]*Xtr[,n-2]+rnorm(N,sd=0.2)
#Xtr=cbind(array(1,c(N,1)),Xtr)




Etr<-NULL

for (r in 1:10000){
  
  ### FORWARD STEP
  z=Xtr
  A=NULL
  ## list of activation vectors
  ## A[[l]] activation vector of the l-th layer of size (N,H[l])
  
  Z=NULL
  ## list of output vectors
  ## Z[[l]] output vector of the l-th layer of size (N,H[l])
  
  for (l in 0:(L-1)){
    #z<-cbind(z,array(1,c(N,1)))
    a=z%*%(W[[l+1]]) ## activation vector of the l-th layer
    if (l<(L-1))
      z=g(a) ## output vector of the l-th layer
    else
      z=g2(a) ## output vector of the L-th layer
    A=c(A,list(a))
    Z=c(Z,list(z))
   
  }
  Yhat=z
  
  E=Ytr-Yhat
  ## training error
  
  if (r %%100==0)
    cat("\n Iteration=",r,"NMSE=",(mean(E^2)/mean((Ytr-mean(Ytr))^2)))
  Etr<-c(Etr,mean(E^2)/mean((Ytr-mean(Ytr))^2))
 
  
  ### BACKPROPAGATION STEP
  Delta=NULL
  Delta[[L+1]]=0
  
  ## DW[[l]] matrix of size (H[l-1],H[l]) 
  DW=NULL
  DW[[L+1]]=0
  
  ## Delta[[l]] matrix of size [H[l],N]
  Delta[[L]]=t(gprime2(A[[L]]))
  ## derivative with respect to W[[L]] weights (output layer)
  DW[[L]]=array(0,dim(W[[L]]))
  for (i in 1:N)
    DW[[L]]=DW[[L]]+E[i]*Z[[L-1]][i,]%*%t(Delta[[L]][,i])
  
  
  for(l in seq(L-1,1,by=-1)){
    Delta[[l]]=(W[[l+1]]%*%(Delta[[l+1]]))*t(gprime(A[[l]]))
    DW[[l]]=array(0,dim(W[[l]]))
    for (i in 1:N){
      if (l>1)
        DW[[l]]=DW[[l]]+E[i]*cbind(Z[[l-1]][i,])%*%rbind(Delta[[l]][,i])
      else
        DW[[l]]=DW[[l]]+E[i]*cbind(Xtr[i,])%*%rbind(Delta[[l]][,i])
    }
  }
  
  eta=0.25/N
  
  
  ## gradient step
  for (l in 1:L)
    W[[l]]=W[[l]]+2*eta*DW[[l]]
  
 
}



par(mfrow=c(1,2))
plot(Ytr,Yhat, xlab='FNN predictions',ylab='Training output')

plot(Etr,xlab='Iterations',ylab='Training Error',type="l")