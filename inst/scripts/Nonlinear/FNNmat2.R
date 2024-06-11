## Matrix based implementation of the FNN backpropagation algorithm
## including bias terms
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




L=8
## number of layers
H=numeric(L)+1
H[1:(L-1)]=round(seq(10,2,length=L-1))
## hidden nodes per layer

## initializazion weights
initsd=1
W<-NULL
for (l in 1:L){
  if (l==1)
    W<-c(W,list(array(rnorm((n)*(H[l]),sd=initsd),c(n,(H[l])))))
  else
    W<-c(W,list(array(rnorm((H[l-1])*(H[l]),sd=initsd),c(H[l-1],H[l]))))
}

## W[[l]] matrix of size (H[l-1],H[l])

Wb<-NULL
for (l in 1:(L))
    Wb<-c(Wb,list(array(rnorm(H[l],sd=initsd),c(1,H[l]))))
## Bias weights: Wb[[l]] matrix of size (1,H[l])


N=250 ## training sample
Xtr=array(rnorm(N*(n),sd=0.5),c(N,n))
Ytr=Xtr[,1]+Xtr[,2]^2-Xtr[,n-1]*Xtr[,n-2]*Xtr[,n]+rnorm(N,sd=0.2)


Etr<-NULL

for (r in 1:50000){
  
  ### FORWARD STEP
  z=Xtr
  A=NULL
  ## list of activation vectors
  ## A[[l]] activation vector of the l-th layer of size (N,H[l])
  
  Z=NULL
  ## list of output vectors
  ## Z[[l]] output vector of the l-th layer of size (N,H[l])
  
  for (l in 0:(L-1)){
    z<-cbind(z,array(1,c(N,1))) ## added column corresponds to unit activation of the bias term
    a=z%*%rbind(W[[l+1]],Wb[[l+1]]) ## activation vector of the l-th layer
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
  
  DWb=NULL
  DWb[[L+1]]=0
  
  
  ## Delta[[l]] matrix of size [H[l],N]
  Delta[[L]]=t(gprime2(A[[L]]))
  ## derivative with respect to W[[L]] weights (output layer)
  DW[[L]]=array(0,dim(W[[L]]))
  DWb[[L]]=array(0,dim(Wb[[L]]))
  for (i in 1:N){
    DW[[L]]=DW[[L]]+E[i]*Z[[L-1]][i,]%*%t(Delta[[L]][,i])
    DWb[[L]]=DWb[[L]]+E[i]%*%t(Delta[[L]][,i])
  }
  
  for(l in seq(L-1,1,by=-1)){
    Delta[[l]]=(W[[l+1]]%*%(Delta[[l+1]]))*t(gprime(A[[l]]))
    DW[[l]]=array(0,dim(W[[l]]))
    DWb[[l]]=array(0,dim(Wb[[l]]))
    for (i in 1:N){
      if (l>1){
        DW[[l]]=DW[[l]]+E[i]*t(cbind(Delta[[l]][,i]) %*% rbind(Z[[l-1]][i,]))
        DWb[[l]]=DWb[[l]]+E[i]%*%rbind(Delta[[l]][,i])
      } else {
        DW[[l]]=DW[[l]]+E[i]*cbind(Xtr[i,])%*%rbind(Delta[[l]][,i])
        DWb[[l]]=DWb[[l]]+E[i]%*%rbind(Delta[[l]][,i])
      }
    }
  }
  
  eta=0.5/N
  
  
  ## gradient step
  for (l in 1:L){
    W[[l]]=W[[l]]+2*eta*DW[[l]]
    Wb[[l]]=Wb[[l]]+2*eta*DWb[[l]] ## update of bias weights
  }
}



par(mfrow=c(1,2))
plot(Ytr,Yhat, xlab='FNN predictions',ylab='Training output')

plot(Etr,xlab='Iterations',ylab='Training Error',type="l")