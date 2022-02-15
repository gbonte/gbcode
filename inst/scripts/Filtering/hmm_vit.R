hmm.vit<- function(A,B,p,O){
  
  no.obs<-length(O)
  N<-nrow(A)
  delta<-array(0,c(N,no.obs))

# delta[i,t]: highest probability along a single path
# which accounts for the first t observations and ends in state i
  
  psi<-array(0,c(N,no.obs))
  
  Q<-numeric(no.obs)
  
  
######### INITIALIZATION DELTA AND PSI
  for (i in 1:N){
    delta[i,1]<-p[i]*B[i,O[1]]
    psi[i,1]<-0
  }
  
  
  MM<-numeric(N)
  for (t in 2:no.obs){
    for (j in 1:N){
      for (i in 1:N){
        MM[i]<-delta[i,t-1]*A[i,j]
      }
      mx<-max(MM)      #  max{1<=i<=N} [delta[i,t-1] A[i,j]]
      ind<-which.max(MM) 
      psi[j,t]<-ind   # arg max{1<=i<=N} [delta[i,t-1] A[i,j]]
      delta[j,t]<-mx*B[j,O[t]]
    }
  }


  # TERMINATION
  P<-max(delta[,no.obs])
  ind<-which.max(delta[,no.obs])

  # BACKTRACKING
  Q[no.obs]<-ind
  
  for (t in seq(no.obs-1,1,by=-1)){
    Q[t]<-psi[Q[t+1],t+1]
  }
  list(prob=P,states=Q, delta=delta,psi=psi)
}
