#E[min f (w)] vs min(E[f(w)])
rm(list=ls())

## random function
f<-function(x,w=0){
 mean((1/60*x*(-60* w *(-210 + 107* w + 17* w^2 - 11* w^3 + w^4) 
          - 30* (210 + 17* w^2 - 22 *w^3 + 3* w^4) *x +
            20 *(107 + 17* w - 2* w^3)* x^2 +
            15* (17 - 22* w + 2* w^2) *x^3 +
            12* (-11 + 3* w) *x^4 + 10 *x^5)))
}

## leastsquares function
lsf<-function(x,w=0){
  sum((x-10*w)^2)#+w*(-sum((x-10*w)^2))
}


n=10 ## size parameters
R<-1000
maxS<-500 # number random search steps

sdw=0.5

seqS=seq(50,maxS,by=100)
localD=0.1
lSearch=1 # the larger, the largeQr the selection bias
grid=TRUE
DX=NULL
DY=NULL

minEY<-NULL

## computation min[E]
besty<-Inf
set.seed(0)
bestExm<-runif(n,-localD,localD)
for (s in 1:maxS){
  Y=NULL
  set.seed(s)
  
  xm=bestExm+runif(n,-localD,localD)
  
  ## local search step
  for (r in 1:R){
    set.seed(r)
    w=rnorm(n,sd=sdw)
    ym=f(xm,w)
    Y=c(Y,ym)
  }
  
  if (mean(Y) <besty){
    besty<-mean(Y)
    bestExm<-xm
    
  }
}
B2<-besty


for (S in seqS){
  RY=NULL
  B<-NULL
  BX<-NULL
  SX<-NULL
  ## computation E[min]
  for (r in 1:R){
    set.seed(r)
    w=rnorm(n,sd=sdw)
    besty<-Inf
    set.seed(0)
    bestxm<-runif(n,-localD,localD)
    for (s in 1:S){
      set.seed(s)
      if (runif(1)<lSearch)
        xm=bestxm+runif(n,-localD,localD)
      else
        xm=runif(n,-localD,localD)
      ## local search step
      
      if (r==1)
        SX<-rbind(SX,xm)
      #else
      if (grid)
        xm=SX[s,]
      ym=f(xm,w)
      if (ym <besty){
        besty<-ym
        bestxm<-xm
        realY<-NULL
        
      }
    } ## for s
    realY<-NULL
    for (rr in 1:R){
      set.seed(rr)
      ww=rnorm(n,sd=sdw)
      ym=f(bestxm,ww)
      realY=c(realY,ym)
    }
    RY<-c(RY,mean(realY))
    B<-c(B,besty)
    BX<-rbind(BX,bestxm)
  } ## for r
  ## mean(B)= E[min]
  
  
 
  
  cat("E [min]=",mean(B),"\n") # E_w [min_x f(x,w)] < min (E)
  cat("min E=",B2,"\n") ## min_x(E [f(x,w)])
  cat("S=",S, " Delta y=",B2-mean(B), "Delta real=", mean(RY)-B2, "bestreal=", mean(RY), 
      "Deltax=", mean((apply(BX,2,mean)-bestExm)^2),"Deltax var=", mean((apply(BX,2,sd))),"\n")
  #if (mean(RY)<B2)
  #  browser()
  DY=c(DY,B2-mean(B))
  minEY<-c(minEY,B2)
  DX=c(DX,mean((apply(BX,2,mean)-bestExm)^2))
}
par(mfrow=c(1,3))
plot(seqS,DY)
plot(seqS,DX)
plot(seqS,minEY)
#X<-seq(-5,7,by=.5)
#plot(X,apply(cbind(X),1,f,0),type='l',lwd=3,col="red")
#for ( r in 1:10)
#  lines(X,apply(cbind(X),1,f,rnorm(1,sd=sdw)))