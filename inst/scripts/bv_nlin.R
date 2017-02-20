# Linear model
# Nonlinear function



par(ask=TRUE)
X<-seq(-10,10,by=1)

beta0<--1
beta1<-1
N<-length(X)
R<-1000
sd.w<-1
beta.hat.1<-numeric(R)
beta.hat.0<-numeric(R)

for (r in 1:R){
  Y<-beta0+beta1*X+X^2+rnorm(N,sd=sd.w)
  x.hat<-mean(X)
  y.hat<-mean(Y)
  S.xy<-sum((X-x.hat)*Y)
  S.xx<-sum((X-x.hat)^2)
  
  beta.hat.1[r]<-S.xy/S.xx
  beta.hat.0[r]<-y.hat-beta.hat.1[r]*x.hat
}

var.beta.hat.1<-(sd.w^2)/S.xx
var(beta.hat.1)
print(paste("Theoretical var beta1=", var.beta.hat.1, "; Observed =",
            var(beta.hat.1) ))
hist(beta.hat.1, main=paste("beta1=", beta1))

var.beta.hat.0<-(sd.w^2)*(1/N+(x.hat^2)/S.xx)
var(beta.hat.0)
print(paste("Theoretical var beta0=", var.beta.hat.0, "; Observed =",
            var(beta.hat.0) ))

hist(beta.hat.0,main=paste("beta0=", beta0))

