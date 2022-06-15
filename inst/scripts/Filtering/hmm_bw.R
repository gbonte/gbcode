hmm.bw<- function (A,B,p,o,no.it=10){
# HMM Baum-Welch algorithm

T<-length(o)
S<-nrow(A)
M<-ncol(B)


alpha<-array(0,c(S,T))
beta<-array(0,c(S,T))
gamma<-array(0,c(S,T))
Sa<-numeric(T)
Sb<-numeric(T)
P.OL<-numeric(T)
xi<-array(0,c(S,S,T))
p.hat<-array(0,c(S,1))
A.hat<-array(0,c(S,S))
B.hat<-array(0,c(S,M))

Lik<-numeric(no.it)

for (it in 1:no.it){
  for (i in 1:S){
    alpha[i,1]<-p[i]*B[i,o[1]]
  }
  
  for (k in 2:T){
    for (j in 1:S){
      alpha[j,k]<-0
      for (i in 1:S){
        alpha[j,k]<-alpha[j,k]+alpha[i,k-1]*A[i,j]
      }
      alpha[j,k]<-alpha[j,k]*B[j,o[k]]
    }
   
  }
  
  
  for (i in 1:S){
    beta[i,T]<-1
  }
 
  
  for (k in seq(T-1,1,by=-1)){
    for (i in 1:S){
      beta[i,k]<-0
      for (j in 1:S){
        beta[i,k]<-beta[i,k]+beta[j,k+1]*A[i,j]*B[j,o[k+1]]
      }
    }
    
  }
  
  for (k in 1:(T-1)){
    P.OL[k]<-0
    for (i in 1:S){
      for (j in 1:S){
        P.OL[k]<-P.OL[k]+alpha[i,k]*A[i,j]*B[j,o[k+1]]*beta[j,k+1]
      }
    }
  }
  
  for (k in 1:(T-1)){
    for (i in 1:S){
      for (j in 1:S){
        xi[i,j,k]<-alpha[i,k]*A[i,j]*B[j,o[k+1]]*beta[j,k+1]/P.OL[k]
      }
      gamma[i,k]<-sum(xi[i,,k])
      
    }
  }
  
  for (i in 1:S){
    gamma[i,T]<-0
    for (j in 1:S)
      gamma[i,T]<-gamma[i,T]+xi[j,i,T-1]
  }
  
  for (i in 1:S){
    p.hat[i,1]<-gamma[i,1]
    for (j in 1:S){
      s.xi<-0
      s.ga<-0
      for (k in 1:(T-1)){
        s.xi<-s.xi+xi[i,j,k]
        s.ga<-s.ga+gamma[i,k]
      }
      A.hat[i,j]<-s.xi/s.ga
    }
  }
  
  for (j in 1:S){
    
    for (m in 1:M){
      s.ga.num<-0
      s.ga<-0
      for (k in 1:(T)){
        s.ga.num<-s.ga.num+gamma[j,k]*as.numeric(o[k]==m)
        s.ga<-s.ga+gamma[j,k]
      }
      B.hat[j,m]<-s.ga.num/s.ga
    }
  }
 
  A<-A.hat
  
  B<-B.hat
  p<-p.hat
  
  Lik[it]<-hmm.ev(A,B,p,o,scale=FALSE)$prob
  
}
                                        #p.hat<-p.hat/(sum(p.hat))
list(A=A.hat,B=B.hat,p=p.hat, lik=Lik)
}
