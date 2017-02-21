## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi
# bayes_gau.R
# Bayesian parametric estimation of the mean of a normal variable with known variance 
######################################################


par(ask=TRUE)
sigma <- 2   # Knwon standard deviation
theta0 <- 13 # A poriori mean
sigma0 <- 1  # A priori standard deviation
Nmax <- 100
DN<-rnorm(Nmax,0,sigma) # Data generation



# Computation of a posteriori distribution
# ========================================
for (N in 3:Nmax){
  
  # Plot of a priori theta estimation
  # ============================================
  I<- seq(-20,20,by=.1)
  plot(I,dnorm(I,theta0,sigma0),type='l',ylim=c(0,1),col="red",main=paste("# observations=",N))
  
  
  # Plot of sampled data
  # ===================================
  zeros<-array(0,dim=c(N,1))
  points(DN[1:N],zeros,col='black')
  
  
  # Plot of a posteriori theta estimation
  # ===================================================
  mu.hat<-mean(DN[1:N])
  sigma1<-sqrt(((sigma^2)/N)*((sigma0^2)/(sigma0^2+((sigma^2)/N))))
  theta1<-(sigma1^2)*(theta0/(sigma0^2)+N*mu.hat/(sigma^2))
  lines(I,dnorm(I,theta1,sigma1),col='blue')
  legend(12,.8,c("Prior","Posterior"),col=c("red","blue"),lty=1)
}


