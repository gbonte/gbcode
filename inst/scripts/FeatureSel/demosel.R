rm(list=ls())
library(gbcode)
ACC<-NULL
ACC2<-NULL
ACC3<-NULL
for ( r in 1:100){
  N=sample(200:350,1)
  neff<-sample(3:10,1)
  n<-sample((neff+1):(10*neff),1)
  
  
  R<-regrDataset(N,n,neff,runif(1,0.1,0.25),seed=r+11)
  X<-R$X
  Y<-R$Y
  feat<-R$feat
  
  fs=NULL
  XX<-array(NA,c(3*N,n))
  for (j in 1:n)
    XX[,j]=sample(X[,j],NROW(XX),replace=TRUE)
  for (nvar in 1:neff){
    V=numeric(n)+Inf
    for (i in setdiff(1:n,fs)){
      Yhat=NULL
      Yhat2=NULL
      for (cc in seq(20,50,by=2)){
        ##XX<-X[,c(fs,i)] #array(rnorm(3*N*(length(fs)+1)),c(3*N,length(fs)+1))
        #Yhat=c(Yhat,pred("lazy",X[,c(fs,i)],Y,X[,c(fs,i)],conPar=c(cc,cc),linPar=NULL,cmbPar=1, class=FALSE))
        Yhat=c(Yhat,pred("lazy",X[,c(fs,i)],Y,XX[,c(fs,i)],conPar=c(cc,cc),linPar=NULL,cmbPar=1, class=FALSE))
        Yhat2=c(Yhat2,pred("lazy",X[,c(fs,i)],Y^2,XX[,c(fs,i)],conPar=c(cc,cc),linPar=NULL,cmbPar=1,class=FALSE))
        #Yhat3=pred("lazy",X[,c(fs,i)],Y^2-Y,X[,c(fs,i)],conPar=c(3,20),linPar=NULL,cmbPar=10,class=FALSE)
      }
      V[i]=mean(Yhat2-Yhat^2)
    }
    fs<-c(fs,which.min(V))
  }
  
  ACC=c(ACC,length(intersect(feat,fs))/neff)
  fs2=rfrank(X,Y,neff)
  ACC2=c(ACC2,length(intersect(feat,fs2))/neff)
  fs3=mrmr(X,Y,neff)
  ACC3=c(ACC3,length(intersect(feat,fs3))/neff)
  if (length(ACC)>12)
    cat("n=",n, "neff=",neff,
        "ACC=",mean(ACC),"ACC2=",mean(ACC2),"ACC3=",mean(ACC3),"p.v=",t.test(ACC,ACC2,paired=TRUE)$p.value,'\n')
  else
    cat("n=",n, "neff=",neff,
    "ACC=",mean(ACC),"ACC2=",mean(ACC2),"ACC3=",mean(ACC3),'\n')
  
  #browser()
}