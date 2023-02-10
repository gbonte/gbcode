
aR=NULL
aM=NULL
for (i in 1:100){
  set.seed(i)
  N<-sample(100:400,1)
  n<-sample(10:200,1)
  neff<-min(round(n/2),50)
  R<-regrDataset(N,n,neff,runif(1,0.1,0.5),seed=i)
  X<-R$X
  Y<-R$Y
  real.features<-R$feat
  ranked.features<-ridgeFsel(X,Y,nmax=neff,alpha=0.5)
  ranked.features2<-mrmr(X,Y,nmax=neff)
  aR=c(aR,length(intersect(real.features,ranked.features))/neff)
  aM=c(aM,length(intersect(real.features,ranked.features2))/neff)
  cat("% correct selection Ridge=",mean(aR),": ","MRMR=", mean(aM),"\n")
}