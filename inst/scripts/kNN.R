##INFO-F-528 Machine learning methods for bioinformatics
## Exercise session 4

KNN<- function(X,Y,k,q){
  l<-levels(Y)
  N<-nrow(X)
  d<-sqrt(apply((X-array(1,c(N,1))%*%q)^2,1,sum)) ## Euclidean metric
  ## d<-sqrt(apply(abs(X-array(1,c(N,1))%*%q),1,sum)) ## Manhattan metric
  ##  d<-1/cor(t(X),q)           ## correlation metric

  index<-sort(d,index.return=TRUE)
  cnt<-numeric(length(l))
  for (i in 1:k){
    cnt[Y[index$ix[i]]]<-cnt[Y[index$ix[i]]]+1
    
  }
  l[which.max(cnt)]
 
}


