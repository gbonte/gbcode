## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

set.seed(0)
cnt<-1
perc<-NULL
mu<-10
sigma<-1
N<-10
alpha<-0.1
z.alpha<-qnorm(alpha/2, lower=FALSE)

seq.N.iter<-seq(10,10000,by=100)
for (N.iter in seq.N.iter){
  mu.hat<-array(0,dim=c(N.iter,1))
  ins<-mu.hat
  for ( i in seq(1,N.iter)){
    D<-rnorm(N,mean=mu,sd=sigma);
    mu.hat[i,1]<-mean(D)
    ins[i,1]<-(mu.hat[i,1]>(mu-z.alpha*sigma/sqrt(N)))& (mu.hat[i,1]<(mu+z.alpha*sigma/sqrt(N)));
  }
  
  perc<-cbind(perc,sum(ins)/N.iter)
  cnt<-cnt+1
}

one<-array(1-alpha,dim=c(length(perc),1))

plot(seq.N.iter,one,ylim=c(0.85,1),xlab="Number iterations",ylab="Frequency of event: ",  main=paste("alpha=",alpha,"N=",N));

lines(seq.N.iter,perc)

