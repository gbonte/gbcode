rm(list=ls())

source("hmm_obs.R")
source("hmm_ev.R")
source("hmm_vit.R")
source("hmm_bw.R")

                                        # probability transition A
N<-5
A<-array(runif(N*N),c(N,N))

for (i in 1:N){
    A[i,]<-A[i,]/sum(A[i,])
  }


# probability output B
M<-5
Sym<-c('a','b','c')
B<-array(runif(N*M),c(N,M))

for (i  in 1:N){
    B[i,]<-B[i,]/sum(B[i,])
  }


p<-array(runif(N),c(N,1))
p<-p/(sum(p))

R<-100
C<-numeric(R)
C.ran<-numeric(R)

for (r in 1:R){
    
    times<-10
    seq.hmm<-hmm.obs(A,B,p,times)
    
   # [A2,B2,p2]=hmm_bw(A,B,p,O);
   
   P.obs<-hmm.ev(A,B,p,seq.hmm$observations,scale=TRUE)

    
   vit<-hmm.vit(A,B,p,seq.hmm$observations)

   C[r]<- sum(vit$states==seq.hmm$states)/times
   Q.ran<-sample(N,times,replace=TRUE)
   C.ran[r]<- sum(Q.ran==seq.hmm$states)/times
   # Q_ran=unidrnd(N,1,T);
   # C(r)=sum(Q==Q_hat)/T;
   # C_ran(r)=sum(Q==Q_ran)/T;
   
  }

print(paste("Viterbi match", mean(C)))
print(paste("Random match",mean(C.ran)))
#(1/N)
