## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())


f<- function(x){
  return(sin(x))
} 
N=100

S=1000

G1=NULL
G2=NULL

xbar=pi/3

sdw=0.5
for (s in 1:S){
  X<-rnorm(N)
  Y=f(X)+rnorm(N,sd=sdw)
  
  Yts=f(xbar)+rnorm(N,sd=sdw)
  Yhat1=0
  Yhat2=mean(Y)
  e1=mean((Yts-Yhat1)^2)
  e2=mean((Yts-Yhat2)^2)
  G1=c(G1,e1)
  G2=c(G2,e2)
}

X<-rnorm(100*N)
Y=f(X)+rnorm(N,sd=sdw)

cat("G1 th=",sdw^2+(f(xbar)^2),"; MC=",mean(G1),"\n")

cat("G2 th=",sdw^2+var(Y)/N+(f(xbar)-mean(Y))^2,"; MC=",mean(G2))