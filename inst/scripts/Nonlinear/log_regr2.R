rm(list=ls())

## h() polynomial functions of different orders 
hlog<-function(x,alpha,ord){
  if (ord==1)
    return(rbind(alpha)%*%cbind(c(1,x)))
  if (ord==2)
    return(rbind(alpha)%*%cbind(c(1,x,x^2)))
  if (ord==3)
    return(rbind(alpha)%*%cbind(c(1,x,x^2,x^3)))
}

predlog<-function(X,alpha,ord=1){
  N=NROW(X)
  P1=NULL
  for (i in 1:N){
    hi=min(10,hlog(X[i,],alpha,ord))
    P1=c(P1,exp(hi)/(1+exp(hi))) 
  }
  return(round(P1))
}

set.seed(0)
### Dataset creation
n=2
mu_0=c(1,-1)
mu_1=c(-1,1)
sdw=1.5
N=200

X0=NULL
for (i in 1:(N))
  X0=rbind(X0,c(rnorm(1,mu_0[1],sdw), rnorm(1,mu_0[2],sdw)))

X1=NULL
for (i in 1:(N))
  X1=rbind(X1,c(rnorm(1,mu_1[1],sdw), rnorm(1,mu_1[2],sdw)))

X=rbind(X0,X1)
Y=c(numeric(N),numeric(N)+1)

I=sample(2*N)
X=X[I,]
Y=Y[I]

Xtr=X[1:N,]
Ytr=Y[1:N]
Xts=X[(N+1):(2*N),]
Yts=Y[(N+1):(2*N)]



###########

R=500
ord=1
p=1+n # number of parameters
alpha=rnorm(p)

### gradient-based search of optimal set of parameters
bestJ=Inf
eta=0.1

### Gradient-based 
for (r in 1:R ){
  
  dJ=numeric(p)
  for (i in 1:N){
    hi=hlog(X[i,],alpha,ord=1)
    dJ[1]=dJ[1]-Y[i]+exp(hi)/(1+exp(hi))
    for (j in 1:n)
      dJ[j+1]=dJ[j+1]-Y[i]*X[i,j]+exp(hi)*X[i,j]/(1+exp(hi))
  }
  ## dJ/da0= -yi+e^hi/(1+e^hi)
  ## dJ/daj= -yi*xij+e^hi*xij/(1+e^hi)
  
  alpha=alpha-eta*dJ/N
  
  Yhat=predlog(Xtr,alpha,ord=1)
  Miscl=length(which(Yhat!=Ytr))/N
  cat( " Miscl error=", Miscl,"\n")
  
}

plot(X[,1],X[,2],pch=19,lwd=3,xlab="x1",ylab="x2", 
     main="Logistic regression classifier")
I0=which(Y==0)
points(X[I0,1],X[I0,2],col="red",pch=19)

Xrange=seq(-7,7,by=0.1)

for (x1 in seq(min(X[,1]),max(X[,1]),length=100))
  for (x2 in seq(min(X[,2]),max(X[,2]),length=100)){
    if (predlog(cbind(x1,x2),alpha,ord)==0)
      points(x1,x2,col="red",pch=".")
    else
      points(x1,x2,col="black",pch=".")
  }






