## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi

# bayes_bin.R
# Bayesian parametric estimation of the binomila probability 
######################################################


par(ask=TRUE)

Nmax <- 1000
p=0.7
DN<-sample(c(0,1),Nmax,prob=c(1-p,p),replace=TRUE) # Data generation


I<- seq(0,1,by=.01)

# Computation of a posteriori distribution
# ========================================
for (N in 3:Nmax){
  
  z=length(which(DN[1:N]==1))
  # Plot of a priori theta estimation
  # ============================================
  
  post=N*choose(N,z)*I^z*(1-I)^(N-z)
  
  # Plot of a posteriori theta estimation
  # ===================================================
  plot(I,post,type='l',col="blue",ylab="posterior distribution",xlab="theta",
       main=paste("N=",N, "theta=",p, "thetahat(freq)=",round(z/N,3), 
                  "thetahat(bay)=",round(I[which.max(post)],3)))
  
  lines(I,numeric(length(I))+1,type='l',col="red",)
  
  
  # Plot of sampled data
  # ===================================
  zeros<-array(0,dim=c(N,1))
  points(DN[1:N],zeros,col='black')
  
  
  
  
  legend("topright",c("Prior","Posterior"),col=c("red","blue"),lty=1)
}


