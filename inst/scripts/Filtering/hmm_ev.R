# Evaluation algorithm:
# given an HMM model lambda=(A,B,p) and an observation sequence o
# it computes the probability P(o|lambda)

hmm.ev <- function(A,B,p,o,scale=FALSE){
  
  T<-length(o)
  # number observations sequence
  
  N<-nrow(A)
  # number states
  
  alpha<-array(0,c(N,T))
  # alpha[i,t]: P(i,o[1:t]): joint probability that at time t we are
  #             at state i and we have observed o[1:t]
  
  
  beta<-array(0,c(N,T))
  # beta[i,t]: P(o[t+1:T]|i): conditional probability of observing
  #           the sequence o[t+1:T] given that we are at state i at time t
  
  
  Sa<-numeric(T)
  Sb<-numeric(T)
  S<-numeric(N)
  
  ############## FORWARD PROCEDURE
  
  for (i in 1:N){
    alpha[i,1]<-p[i]*B[i,o[1]]
  }
  
  if (scale) {
    S[1]<-sum(alpha[,1])
    alpha[,1]<-alpha[,1]/S[1]
  }
  
  # Initialization alpha: alpha[i,1]=p(i)*B[i,o[1]]
  
  for (t in 2:T){
    for (j in 1:N){
      alpha[j,t]<-0
      for (i in 1:N){
        alpha[j,t]<-alpha[j,t]+alpha[i,t-1]*A[i,j]
      }
      alpha[j,t]<-alpha[j,t]*B[j,o[t]]
    }
    if (scale){
      S[t]<-sum(alpha[,t])
      alpha[,t]<-alpha[,t]/S[t]
    }
  }
  
  P<-sum(alpha[,T])
  if (scale){
    P<-1/prod(1/S)
  }
  
  ############## BACKWARD PROCEDURE
  if (! scale){
    for (i in 1:N){
      beta[i,T]<-1;
    }
    # Initialization beta: beta[i,T]=1
    
    
    for (t in seq(T-1,1,by=-1)){
      
      for (i in 1:N){
        beta[i,t]<-0
        for (j in 1:N){
          beta[i,t]<-beta[i,t]+beta[j,t+1]*A[i,j]*B[j,o[t+1]]
        }
      }
    }
    
    P2<-0
    for (i in 1:N){
      P2<- P2+beta[i,1]*alpha[i,1]
    }
    list(prob=P,alpha=alpha,beta=beta)
  }
  else
    list(prob=P,alpha=alpha)
  
}

