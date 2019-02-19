R=10000

muz=1
sdz=0.5
N=20
muhat<-NULL
sdhat<-NULL
muhat2<-NULL
sdhat2<-NULL
for (r in 1:R){
  
  DN=rnorm(N,muz,sdz)
  xaxis=seq(muz-3*sdz,muz+3*sdz,by=0.1)
  par(mfrow=c(3,2)) 
  plot(DN,DN*0,main=paste(" mu=",muz," sd=",sdz," N=",N),ylab="",xlab="Dataset")
  plot(xaxis,dnorm(xaxis,muz,sdz),main="z density",type="l",xlab="z")
  muhat<-c(muhat,mean(DN))
  sdhat<-c(sdhat,sd(DN))
  muhat2<-c(muhat2,min(DN))
  sdhat2<-c(sdhat2,0.5*(max(DN)-min(DN)))
  
  if (r %% 10==0){
    if (r>30)
      br=r/15
    else
      br=r/2
    hist(muhat,breaks=br,freq=FALSE,
         main=paste("Bias=",round(mean(muhat)-muz,2),"Var=",round(var(muhat),2)),
         xlab="Sample average distribution")
    hist(sdhat,breaks=br,freq=FALSE,
         main=paste("Bias=",round(mean(sdhat)-sdz,2),"Var=",round(var(sdhat),2)), 
         xlab="Sample stdev distribution")
    
    hist(muhat2,breaks=br,freq=FALSE,
         main=paste("Bias=",round(mean(muhat2)-muz,2),"Var=",round(var(muhat2),2)), 
         xlab="Sample min distribution")
    hist(sdhat2,breaks=br,freq=FALSE,
         main=paste("Bias=",round(mean(sdhat2)-sdz,2),"Var=",round(var(sdhat2),2)), 
         xlab="Sample width distribution")
    
    readline(prompt = paste("r=",r , " Make more runs..."))
  }
}