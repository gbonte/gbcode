## Viterbi vs random filtering
rm(list=ls())

source("hmm_obs.R")
source("hmm_ev.R")
source("hmm_vit.R")


set.seed(2)
S<-5
M<-5

# random probability transition A
A<-array(runif(S*S),c(S,S))
for (i in 1:S){
  A[i,]<-A[i,]/sum(A[i,])
}


# random probability output B
B<-array(runif(S*M),c(S,M))

for (i  in 1:S){
  B[i,]<-B[i,]/sum(B[i,])
}

## initial state distribution
p<-array(runif(S),c(S,1))
p<-p/(sum(p))

R<-100
C<-numeric(R)
C.ran<-numeric(R)

for (r in 1:R){
  
  TT<-200
  seq.hmm<-hmm.obs(A,B,p,TT)
  
  # Viterbi filtering  
  vit<-hmm.vit(A,B,p,seq.hmm$observations)
  
  C[r]<- sum(vit$states==seq.hmm$states)/TT
  Q.ran<-sample(S,TT,replace=TRUE)
  C.ran[r]<- sum(Q.ran==seq.hmm$states)/TT
  
}

print(paste("Viterbi match", mean(C)))
print(paste("Random match",mean(C.ran)))

