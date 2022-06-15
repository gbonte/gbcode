rm(list=ls())
source("C:/bontempi/prof/bioinfo/hmm_vit.R")
source("C:/bontempi/prof/bioinfo/hmm_ev.R")

N<-2
M<-2
A<-array(c(.9,.8,.1,.2),c(N,N))
B<-array(c(1/2,1/4,1/2,3/4),c(N,M))
p<-c(0.5,0.5)


O<-c(2,2,2)
S<-NULL
PR<-NULL
for (q1 in 1:N){
  for (q2 in 1:N){
    for (q3 in 1:N){
      S<-cbind(S,c(q1,q2,q3))
      pr<-p[q1]*B[q1,O[1]]*A[q1,q2]*B[q2,O[2]]*A[q2,q3]*B[q3,O[3]]
      PR<-c(PR,pr)
    }
  }
}



P.eval<-0
for (i in 1:(N^length(O))){
  P.path<-p[S[1,i]]*B[S[1,i],O[1]]
  for (j in 2:length(O)){
    P.path<-P.path*A[S[j-1,i],S[j,i]]*B[S[j,i],O[j]]
  }
 
  cat("Proba P(O,[",S[,i],"])=",P.path,"\n") 
  P.eval<-P.eval+P.path
  
}
print(paste("Probability of O given the model (exhaustive)",P.eval))


eva<-hmm.ev(A,B,p,O)

print(paste("Probability of O given the model (forward)",eva$prob))



wh<-which.max(PR)
print("Optimal state sequence (exhaustive):")
print(S[,wh])

vit<-hmm.vit(A,B,p,O)
print("Optimal state sequence (Viterbi):")
print(vit$states)



