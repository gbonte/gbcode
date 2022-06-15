## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi


M=100 # number of alternatives

x=rnorm(M)

mu=x^2

N=10 # number of samples
sdw=0.2
R=100

mbest=x[which.min(mu)]
best=min(mu)
BestHat=NULL
for (r  in 1:R){
   D<-NULL
   for ( m in 1:M)
      D<-rbind(D,rnorm(N,mu[m],sd=sdw))
   mbesthat<-which.min(apply(D,1,mean))
   besthat<-min(apply(D,1,mean))
   BestHat=c(BestHat,besthat)
}

(mean(BestHat)) # E[min]
(min(mu)) # min[E]

print(mean(BestHat)-min(mu))