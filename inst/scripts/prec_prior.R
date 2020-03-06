rm(list=ls())

ASSCL2<-function(mu0,mu1,sd0,sd1,p1,thr,N=10000){
  
  p0=1-p1
  
  
  N0=round(N*p0)
  N1=round(N*p1)
  
  D0<-rnorm(N0,mu0,sd0)
  D1<-rnorm(N1,mu1,sd1)
  
  X<-c(D0,D1)
  Y<-c(numeric(N0),numeric(N1)+1)
  
  Ihat0<-which(X<thr)
  Ihat1<-which(X>=thr)
  I0<-1:N0
  I1<-setdiff(1:length(Y),I0)
  
  TP<-length(intersect(I1,Ihat1))
  FP<-length(intersect(I0,Ihat1))
  TN<-length(intersect(I0,Ihat0))
  FN<-length(intersect(I1,Ihat0))
  
  TPR<-TP/(TP+FN)
  TNR<-TN/(TN+FP)
  
  PR<-0
  if ((TP+FP)>0)
    PR<-TP/(TP+FP)
  
  return(list(PR=PR,TPR=TPR,FPR=1-TNR,TNR=TNR,ACC=(TP+TN)/N))
  
  
}


ASSCL<-function(mu0,mu1,sd0,sd1,p1,thr,N=10000){
  
  p0=1-p1
  
  
  
  TPR<-pnorm(thr,mu1,sd1,lower.tail=FALSE)
  FPR<-pnorm(thr,mu0,sd0,lower.tail=FALSE)
  TNR<-pnorm(thr,mu0,sd0)
  FNR<-pnorm(thr,mu1,sd1)
  
  
  
  PR<-TPR*p1/(TPR*p1+FPR*p0)
  
  return(list(PR=PR,TPR=TPR,TNR=TNR,FPR=1-TNR))
  
  
}



mu0=0
sd0=0.5

mu1=0.4

sd1=0.5
Sp1=seq(0.05,0.2,by=0.005)
Sthr=seq(mu0,2*mu1,by=0.01)

p1=0.5
ASSD<-ASSCL2(mu0,mu1,sd0,sd1,p1,0.2,N=10000)
  
ASSD$PR
ASSD$TPR*p1/(ASSD$TPR*p1+(1-ASSD$TNR)*(1-p1))

ASSD$ACC
ASSD$TPR*p1+ASSD$TNR*(1-p1)


browser()
for (p1 in Sp1){
  Spr=NULL
  Stpr=NULL
  Sfpr=NULL
  for (thr in Sthr){
    AS<-ASSCL(mu0,mu1,sd0,sd1,p1,thr=thr,N=20000)
    Spr<-c(Spr,AS$PR)
    Stpr<-c(Stpr,AS$TPR)
    Sfpr<-c(Sfpr,AS$FPR)
    
  }
  if (p1==Sp1[1])
    plot(Sthr,Spr)
  else
    lines(Sthr,Spr)
  
}
browser()
for (p1 in seq(0.05,1,by=0.005)){
  
  
  Spr=NULL
  for (thr in Sthr){
    Spr<-c(Spr,ASSCL(mu0,mu1,sd0,sd1,p1,thr=thr)$PR)
    
  }
  thr<-Sthr[which.max(Spr)]
  ass<-ASSCL(mu0,mu1,sd0,sd1,p1,thr=thr)
  cat("p1=", p1,"thr=",thr,"PR=",ass$PR," ")
  
  PRstar=ass$PR
  eps=0.001
  
  OKdelta<-NULL
  OKPR<-NULL
  OKthr<-NULL
  MINR=1.1
  for (delta in seq(-p1+eps,0,length=50)){
    
    bestdiff=Inf
    for (thr2 in seq(0,1,by=0.01)){
      PR<-ASSCL(mu0,mu1,sd0,sd1,min(1,max(0,p1+delta)),thr2)$PR
      if (abs(PR-PRstar) < bestdiff){
        bestthr=thr2
        bestdiff=abs(PR-PRstar)
        bestPR=PR
        if (bestdiff<0.005){
          OKdelta<-c(OKdelta,delta)
          OKPR<-c(OKPR,PR)
          OKthr<-c(OKthr,thr2)
          break;
        }
      }
      
    } ## for thr1
    
    
  } ## for delta
  if (length(OKdelta)>0)
    cat("max delta=",OKdelta[which.max(abs(OKdelta))], 
        "adthr=",OKthr[which.max(abs(OKdelta))],
        "adPR=",OKPR[which.max(abs(OKdelta))]," \n")
  else
    cat("\n")
}
#cat("bestthr=",bestthr,"bestdiff=",bestdiff,"bestPR=",bestPR,"PRstar=", PRstar,"\n")
