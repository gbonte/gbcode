## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())

mx<-function(x,mu1,mu2,w=0.5){
  w*dnorm(x,mu1,0.5)+(1-w)*dnorm(x,mu2,0.5)
}

## negative class distribution
px.Minus<-function(x){
  mx(x,mu1=-1,mu2=1)
}

px.Plus<-function(x){
  dnorm(x)
}

X<-seq(-3,3,length=100)
thr=-1
plot(X,px.Minus(X),type="l",col="red")
lines(X,px.Plus(X),col="green")
abline(v=thr)

pP<-0.2
pN<-1-pP

pTP<-integrate(px.Plus,thr,100)$value
pTN<-integrate(px.Minus,-100,thr)$value
pFP<-integrate(px.Minus,thr,100)$value
pFN<-integrate(px.Plus,-100,thr)$value

cat("\n -- \n pTP=",pTP*pP,"pTN=",pTN*pN, 
    "\n pFP=",pFP*pN,"pFN=",pFN*pP,"\n")

N=1000

cat("\n -- \n TP=",N*pTP*pP,"TN=",N*pTN*pN, 
    "\n FP=",N*pFP*pN,"FN=",N*pFN*pP,"\n",
    " prec=",N*pTP*pP/(N*pTP*pP+N*pFP*pN),"\n" )