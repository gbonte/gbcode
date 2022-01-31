## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

KNN<- function(X,Y,k,q){
  
  Y=factor(Y)
  l<-levels(Y)
  N<-nrow(X)

  if (is.vector(X)){
    d<-sqrt((X-q)^2)}
    #d<-1/cor(X,q)}
  else{
  	d<- apply(X, 1, function(x) sqrt(sum((x-q)^2))) } ## Euclidean metric
	#d<- apply(X, 1, function(x) 1/(1+cor(x,q)))}
	#d<- apply(X, 1, function(x) sum(abs(x-q))) }
	
  index<-sort(d,index.return=TRUE)
  cnt<-numeric(length(l))
  
  for (i in 1:k){
    cnt[Y[index$ix[i]]]<-cnt[Y[index$ix[i]]]+1
  }

  l[which.max(cnt)]
}



data(golub)
X<-golub$X
Y=golub$Y
n<-ncol(X)
N<-nrow(X)
set.seed(0)
I<-sample(N)
X<-X[I,]
Y<-Y[I]
K<-1

plot(seq(1,50),rep(0,50),ylim=c(0,0.3),type="n",ylab="misclassification loo",xlab="size")
for ( size in c(2:50)){
  ###############################
  # Leave-one-out
  Y.hat.ts<-numeric(N)
  for (i in 1:N) { #for each of the N samples
    X.tr<-X[-i,]
    Y.tr<-Y[-i]
    q<-X[i,]
    
    correlation<-cor(X.tr,as.numeric(Y.tr))
    ranked.var<-sort(abs(correlation),decreasing=TRUE,index.return=TRUE)$ix
    
    Y.ts<-Y[i]
    Y.hat.ts[i] <- KNN(X.tr[,ranked.var[1:size]], Y.tr,K,q[ranked.var[1:size]])  
  }
  miscl.loo<-mean(Y.hat.ts!=Y)
  points(size,miscl.loo,pch=16,cex=0.6)
}