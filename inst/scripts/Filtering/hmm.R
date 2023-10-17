rm(list=ls())
source("hmm_vit.R")
source("hmm_ev.R")

S<-3 ## number of states
M<-3 ## number of observation symbols
A<-t(array(c(.5,.25,.25,.1,.2,.7,0.5,0.25,0.25),c(S,S))) ## state transition
B<-t(array(c(1/2,1/4,1/4,1/4,1/4,1/2,0.1,0.8,0.1),c(M,S)))## observation symbol
p<-c(0.5,0.4,0.1) ## initial state distribution


O<-c(1,2,3,1)
SS<-NULL
PR<-NULL
for (q1 in 1:S){
  for (q2 in 1:S){
    for (q3 in 1:S){
      for (q4 in 1:S){
        SS<-cbind(SS,c(q1,q2,q3,q4))
        pr<-p[q1]*B[q1,O[1]]*A[q1,q2]*B[q2,O[2]]*A[q2,q3]*B[q3,O[3]]*A[q3,q4]*B[q4,O[4]]
        PR<-c(PR,pr)
      }
    }
  }
}



P.eval<-0
for (i in 1:(S^length(O))){
  P.path<-p[SS[1,i]]*B[SS[1,i],O[1]]
  for (j in 2:length(O)){
    P.path<-P.path*A[SS[j-1,i],SS[j,i]]*B[SS[j,i],O[j]]
  }
  
  cat("Proba P(O,[",SS[,i],"])=",P.path,"\n") 
  P.eval<-P.eval+P.path
  
}
print(paste("Probability of O given the model (exhaustive)",P.eval))


eva<-hmm.ev(A,B,p,O)

print(paste("Probability of O given the model (forward)",eva$prob))



print(paste("Probability of O given the model (backward)",eva$prob2))

wh<-which.max(PR)
print("Optimal state sequence (exhaustive):")
print(SS[,wh])

vit<-hmm.vit(A,B,p,O)
print("Optimal state sequence (Viterbi):")
print(vit$states)



