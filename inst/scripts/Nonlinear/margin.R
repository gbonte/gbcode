N=1000
Y=sample(c(-1,1),N,rep=TRUE)
h=runif(N,-4,4)

M=h*Y
iM=sort(M,decreasing=FALSE,index.return=TRUE)$ix

plot(M[iM],(h[iM]-Y[iM])^2,col="red",type="l", 
     xlab="Margin",ylab="losses")
lines(M[iM],exp(-M[iM]))