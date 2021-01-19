## Script Explaining Away effect

## index in CP Tables 1-> True, 2-> False
rm(list=ls())
P.C.1=0.1 ## a priori probability that C is true
P.C=c(P.C.1,1-P.C.1)

P.V.1= 0.6 ## a priori probability that V is true

P.V=c(P.V.1,1-P.V.1)

P.H.CV<-array(0,c(2,2,2)) ## CPT of H given C and V

P.H.CV[1,1,1]=0.95 
P.H.CV[1,1,2]=0.8
P.H.CV[1,2,1]=0.8
P.H.CV[1,2,2]=0.1

P.H.CV[2,,]=1-P.H.CV[1,,]

Jo<-array(NA,c(2,2,2)) ## Jo[H,C,V]

for (H in 1:2){
  for (C in 1:2){
    for (V in 1:2){
      Jo[H,C,V]=P.H.CV[H,C,V]*P.C[C]*P.V[V]
    }
    
  }
  
}
print(sum(Jo))  ## Check that the joint probability is normalized


## Jo[H,C,V]

### Independence of C and V
P.C.1.V.1=sum(Jo[,1,1])    # P(C=1 & V=1)
cat("P(C=T)=",P.C.1, "; P(C=T|V=T)=",P.C.1.V.1/P.V.1,"\n")

###

P.H.1=sum(Jo[1,,])    # P(H=1)
P.C.1.H.1=sum(Jo[1,1,])  # P(C=1 & H=1)
cat("P(C=T|H=T)=",P.C.1.H.1/P.H.1,"\n")

###
P.H.1.V.1=sum(Jo[1,,1])    # P(H=1 & V=1)
P.C.1.H.1.V.1=Jo[1,1,1]  # P(H=1 & C=1 & V=1)
cat("P(C=T|H=T,V=T)=",P.C.1.H.1.V.1/P.H.1.V.1,"\n")

###
P.H.1.V.0=sum(Jo[1,,2])    # P(H=1 & V=1)
P.C.1.H.1.V.0=Jo[1,1,2]  # P(H=1 & C=1 & V=1)
cat("P(C=T|H=T,V=F)=",P.C.1.H.1.V.0/P.H.1.V.0,"\n")

