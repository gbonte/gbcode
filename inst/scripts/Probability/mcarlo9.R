## Birthday problem (from the book 
# Introduction to Probability, Blitzstein and Hwang, page 31  )
## "Birthday problem": probability that in a party with n participants at least two persons
# have the same birthday


R<-10^4 ## number of Monte Carlo trials
Phat<-NULL
seqN<-seq(5,100,by=2)
for (n in seqN){
  ## number of party participants
  r <- replicate(R, max(tabulate(sample(1:365,n,replace=TRUE))))
  phat=sum(r>=2)/R ## MC estimation of probability
  Phat<-c(Phat,phat)
}

plot(seqN,Phat,type="l",xlab="# participants", ylab="Prob")