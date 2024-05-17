rm(list=ls())

## distribution of maximum of N normal variables 
## pN(z)=N[pnorm(z)]^(N-1)dnorm(z)

## distribution of minimum of N normal variables 
## p1(z)=N[1-pnorm(z)]^(N-1)dnorm(z)

muz=2
sdz=2
seqN= seq(5,100,by=5) 
Z=seq(muz-5*sdz,muz+5*sdz,length.out=500)

plot(Z,dnorm(Z,mean=muz,sd=sdz),type="l",ylim=c(0,1),
     lwd=3,ylab="",main=paste("N=", max(seqN)," i.i.d. r.v.s. zi.  Density min(zi) (green) and max(zi) (red)"))

for (N in seqN ){
  pN=NULL
  for (z in Z)
    pN=c(pN,N*pnorm(z,mean=muz,sd=sdz)^(N-1)*dnorm(z,mean=muz,sd=sdz))
  lines(Z,pN,col="red")
  
  p1=NULL
  for (z in Z)
    p1=c(p1,N*(1-pnorm(z,mean=muz,sd=sdz))^(N-1)*dnorm(z,mean=muz,sd=sdz))
  lines(Z,p1,col="green")
  
}

