
rm(list=ls())
set.seed(0)
source("hmm_obs.R")
source("hmm_bw.R")
source("hmm_ev.R")

#probability transition A

S<-2 ## number of states
M<-6 ## size  of the  observation domain

A<-t(array(c(0.95, 0.05, 0.1, 0.9),c(2,2)))
times<-50

# probability output B
B<-t(array(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6, 0.1, 0.1, 0.1, 0.1, 0.1, 0.5),c(6,2)))

p<-c(0.99, 0.01)

seq<-hmm.obs(A,B,p,times)

maxl=-Inf

### Several random initialisations of the BW 
for (rep in 1:500){
  set.seed(rep)
  ## initialisation of BW algorithm
  Ainit<-array(runif(S*S),c(S,S))
  
  for (i in 1:S){
    Ainit[i,]<-Ainit[i,]/sum(Ainit[i,])
  }
  # probability output B
  
  Binit<-array(runif(S*M),c(S,M))
  for (i  in 1:S){
    Binit[i,]<-Binit[i,]/sum(Binit[i,])
  }
  pinit<-runif(1)
  pinit<-c(pinit,1-pinit)
  
  
  est.hmm<-hmm.bw(Ainit,Binit,pinit,
                  seq$observations,no.it=20)
  
  ## selection of the maxlikelihood configuration
  if (!is.nan(max(est.hmm$lik)))
    if (max(est.hmm$lik)>maxl){
      maxl=max(est.hmm$lik)
      print(log(maxl))
      best.hmm=est.hmm
    }
  
  
}

cat("A=")
print(A)

cat("\n Ahat=")
print(best.hmm$A)
cat("B=")
print(B)
cat("Bhat=")
print(best.hmm$B)

plot(log(best.hmm$lik), main="EM Log Likelihood",type="l")