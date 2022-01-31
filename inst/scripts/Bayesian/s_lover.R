rm(list=ls())

par(ask=TRUE)
n.action<-2;
n.state<-2 # number of states

N<-1 # number of observed samples
nZ<-3 # number of values of z 


lambda<-matrix(c(0.3,0.5,0.7, 0.1),nrow=n.action,byrow=TRUE)
# lambda[n.action,n.state]: loss matrix


prior<-c(0.4, 0.6)
# a priori probability [n.state,1]


LIK<-matrix(c(0.5, 0.45, 0.05, 0.2, 0.2, 0.6),nrow=n.state,byrow=TRUE)
# LIK[n.state,n.data]

n.data<-nZ^N
# number of different observable data

delta<-NULL
for (i in 1:(n.data)){
  delta<-c(delta, list(1:n.action))
}

delta<-expand.grid(delta)
# delta[n.dec,n.data]
# action<-delta(dec,data)

n.dec<-dim(delta)[1]
# number of decision rules

R<-array(0,dim=c(n.dec,n.state))
for (dec in (1:n.dec)){
   for (state in (1:n.state)){
     for (data in (1:n.data)){
         R[dec,state]<-R[dec,state]+lambda[delta[dec,data],state]*LIK[state,data]
       }
    }
   plot(R[dec,],xlab="State",ylab="risk",main=paste("Decision rule", dec))
   # pause
 }
R

bR<-array(0,c(n.dec,1))
for (dec in (1:n.dec)){
     for (state in (1:n.state)){
      bR[dec]<-bR[dec]+R[dec,state]*prior[state] #bayes risk
    }
   }

im<-which.min(bR)

delta[im,]


marg<-array(0,c(n.data,1))
for (data in (1:n.data)){
    for (state in (1:n.state)){
      marg[data]<-marg[data]+LIK[state,data]*prior[state]
  }
}

P<-array(0,c(n.state,n.data))
# conditional probability P(state|data)
for (state in (1:n.state)){
   for (data in 1:n.data){
      P[state,data]<-LIK[state,data]*prior[state]/marg[data]
 }
 }

Ra<-array(0,c(n.action,n.data))
# conditional risk R(a|data)

for (data in (1:n.data)){
  for (action in (1:n.action)){
      for (state in (1:n.state)){
      Ra[action,data]<-Ra[action,data]+lambda[action,state]*P[state,data]
    }
  }
}

print(P)
print(Ra)


