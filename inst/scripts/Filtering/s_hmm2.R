rm(list=ls())

source("C:/bontempi/prof/bioinfo/hmm_obs.R")
source("C:/bontempi/prof/bioinfo/hmm_ev.R")
source("C:/bontempi/prof/bioinfo/hmm_vit.R")
source("C:/bontempi/prof/bioinfo/hmm_bw.R")

                                        # probability transition A
set.seed(0)
N<-2
A<-array(runif(N*N),c(N,N))

for (i in 1:N){
    A[i,]<-A[i,]/sum(A[i,])
  }


# probability output B
M<-3

B<-array(runif(N*M),c(N,M))

for (i  in 1:N){
    B[i,]<-B[i,]/sum(B[i,])
  }






A2<-array(runif(N*N),c(N,N))

for (i in 1:N){
    A2[i,]<-A2[i,]/sum(A2[i,])
  }


# probability output B
M<-3

B2<-array(runif(N*M),c(N,M))

for (i  in 1:N){
    B2[i,]<-B2[i,]/sum(B2[i,])
  }


p<-array(runif(N),c(N,1))
p<-p/(sum(p))


p2<-array(runif(N),c(N,1))
p2<-p2/(sum(p2))


R<-500
C<-numeric(R)
CB<-numeric(R)
times<-50
seq<-hmm.obs(A,B,p,times)



est.hmm<-hmm.bw(A2,B2,p2,seq$observations,scale=TRUE,no.it=1000)

plot(est.hmm$lik)
