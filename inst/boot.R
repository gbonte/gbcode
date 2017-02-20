

set.seed(0)


Qb<-NULL
for (N in seq(25,1000,by=25)){
  DN<-runif(N,0,10)
  for (B in seq(10,50,by=5)){
    for (b in 1:B){
      Db<-sample(DN,replace=TRUE)
      Qb<-c(Qb,quantile(Db,probs=0.9))
    }
    #   hist(Qb,main=paste("# obs.=", N, "# bootstrap samples=",B ))
    print(paste("# obs.=", N, ", # bootstrap samples=",B, ", estim=", mean(Qb)))
    
  }
  print("#######################")
}

