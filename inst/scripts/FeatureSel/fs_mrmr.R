rm(list=ls())
perc<-NULL
perc2<-NULL
## percentage of corrected features (True Positive)

for (r in 1:100){
  set.seed(r)
  N<-sample(50:100,1)
  n<-sample(100:200,1)
  neff<-min(n-1,sample(3:10,1))
  sdev=runif(1,0.2,0.5)
  R<-regrDataset(N,n,neff,sdev,seed=runif(1))
  X<-R$X
  Y<-R$Y
  real.features<-R$feat
  ranked.features<-mrmr(X,Y,nmax=neff)
  ranked.features2<-rfrank(X,Y,nmax=neff)
  perc<-c(perc,length(intersect(real.features,ranked.features))/neff)
  perc2<-c(perc2,length(intersect(real.features,ranked.features2))/neff)
  cat("r=", r, "mrmr TPR=",mean(perc)," RF TPR=",mean(perc2) ,"\n")
}