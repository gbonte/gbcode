
hmm.obs <- function(A,B,p,times){

# probability transition A
N<-nrow(A)
M<-ncol(B)

q<-numeric(times)
o<-numeric(times)

q[1]<-sample(N,1,prob=p)
o[1]<-sample(M,1,prob=B[q[1],])

for (t in 2:times){
    q[t]<-sample(N,1,prob=A[q[t-1],])
    o[t]<-sample(M,1,prob=B[q[t],])
  }
list(states=q,observations=o)
}
