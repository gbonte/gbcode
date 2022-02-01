## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi



rm(list=ls())
set.seed(0)
Wx=50 ## width arena
Wy=50

## initialization probability mass
P=array(1/(Wx*Wy),c(Wx,Wy))

S=numeric(4) # Sensors: east, south,west, north
x=Wx/2
y=Wy/2

zero=1e-10
sdS=4
sdX=2
while (1){
  
  # control action
  ux<-sample(c(-2,-1,0,1,2),1)
  uy<-sample(c(-2,-1,0,1, 2),1)
  
  
  # update position robot
  x=max(1,min(x+ux,Wx))
  y=max(1,min(y+uy,Wy))
  
  ## Bayes state update after control action
  for (i in 1:Wx){
    for (j in 1:Wy){
      prev.i=round(max(1,min(i-ux,Wx)))
      prev.j=round(max(1,min(j-uy,Wy)))
       P[i,j]=P[prev.i,prev.j]
      
    }
  }
  P=P/sum(P)
  
  
  ## robot slippery
  x=(max(1,min(x+rnorm(1,sd=sdX),Wx)))
  y=(max(1,min(y+rnorm(1,sd=sdX),Wy)))
 
  
  ### sensor data collection
  S=pmax(pmin(c(Wx-x,y-1,x-1,Wy-y)+rnorm(4,sd=sdS),Wx),0)
  
   
  ## Bayes state update after sensing 
  
  for (i in 1:Wx){
    for (j in 1:Wy){
      Pt=log(P[i,j])
      Lik=dnorm(S[1],mean=Wx-i,sd=sdS,log=TRUE)+dnorm(S[2],mean=j-1,sd=sdS,log=TRUE)+
        dnorm(S[3],mean=i-1,sd=sdS,log=TRUE)+dnorm(S[4],mean=Wy-j,sd=sdS,log=TRUE)
      
      P[i,j]=max(zero,exp(Pt+Lik))
      
    }
  }
  P=P/sum(P)
  
  
  
  ## visualization of robot position vs state density estimation
  image(1:Wx,1:Wy,P, col = grey(seq(0, 1, length = 256)),
        main="Bayes robot state estimation",
        xlab="",ylab="")
  points(x,y,col="red",lwd=5)
  Sys.sleep(0.1)
  
  
}