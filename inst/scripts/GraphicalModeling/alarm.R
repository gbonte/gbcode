## Script Alarm Bayesian Network example

## index in CP Tables 1-> True, 2-> False
rm(list=ls())
pBT=0.001 ## a priori probability that B is true
P.B=c(pBT,1-pBT)

pET= 0.002 ## a priori probability that E is true

P.E=c(pET,1-pET)

P.A.BE<-array(0,c(2,2,2)) ## CPT of A given B and E

P.A.BE[1,1,1]=0.95
P.A.BE[1,1,2]=0.94
P.A.BE[1,2,1]=0.29
P.A.BE[1,2,2]=0.001

P.A.BE[2,,]=1-P.A.BE[1,,] 

P.J.A<-array(0,c(2,2)) ## CPT of J given A
P.J.A[1,1]=0.9
P.J.A[1,2]=0.05
P.J.A[2,]=1-P.J.A[1,]

P.M.A<-array(0,c(2,2)) ## CPT of M given A
P.M.A[1,1]=0.7
P.M.A[1,2]=0.01

P.M.A[2,]=1-P.M.A[1,]


Jo<-array(NA,c(2,2,2,2,2)) ## Jo[J,M,A,E,B]

for (J in 1:2){
  for (M in 1:2){
    for (A in 1:2){
      for (E in 1:2){
        for (B in 1:2){
          Jo[J,M,A,E,B]=P.J.A[J,A]*P.M.A[M,A]*P.A.BE[A,B,E]*P.B[B]*P.E[E]
        }
      }
    }
  }
}
print(sum(Jo))  ## Check that the joint probability is normalized


## Jo[J,M,A,E,B]


P.B.1=sum(Jo[,,,,1])    # P(B=1)
P.A.1.B.1=sum(Jo[,,1,,1])  # P(A=1 & B=1)
cat("P(A=T|B=T)=",P.A.1.B.1/P.B.1,"\n")

###
P.B.0=sum(Jo[,,,,2])    # P(B=0)
P.A.1.B.0=sum(Jo[,,1,,2])  # P(A=1 & B=0)
cat("P(A=T|B=F)=",P.A.1.B.0/P.B.0,"\n")

###

cat("P(A=F|B=F)=",1-P.A.1.B.0/P.B.0,"\n")

###
P.J.1=sum(Jo[1,,,,])    # P(J=1)
P.J.1.B.1=sum(Jo[1,,,,1])  # P(J=1 & B=1)

P.J.1.B.0=sum(Jo[1,,,,2])
cat("P(J=T|B=F)=",P.J.1.B.0/P.B.0,"\n")

cat("P(B=T|J=T)=",P.J.1.B.1/P.J.1,"\n")


