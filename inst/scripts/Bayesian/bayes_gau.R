## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi
# bayes_gau.R
# Bayesian parametric estimation of the mean of a normal variable with known variance 
######################################################


par(ask=TRUE)
sigma <- 2   # Knwon standard deviation
theta0 <- 13 # A priori mean
sigma0 <- 1  # A priori standard deviation
Nmax <- 1000
DN<-rnorm(Nmax,0,sigma) # Data generation



# Computation of a posteriori distribution
# ========================================
for (N in 3:Nmax){
  
  # Plot of a posteriori theta estimation
  # ===================================================
  mu.hat<-mean(DN[1:N])
  sigma1<-sqrt(((sigma^2)/N)*((sigma0^2)/(sigma0^2+((sigma^2)/N))))
  theta1<-(sigma1^2)*(theta0/(sigma0^2)+N*mu.hat/(sigma^2))
  post=dnorm(I,theta1,sigma1)
  plot(I,post,col='blue',main=paste("N=",N,"thetahat(freq)=",round(mu.hat,3), 
                                                      "thetahat(bay)=",round(I[which.max(post)],3)),xlab="theta",
       type='l',ylab="posterior distribution")
  
  
  # Plot of a priori theta estimation
  # ============================================
  I<- seq(-20,20,by=.1)
  lines(I,dnorm(I,theta0,sigma0),ylim=c(0,1),col="red")
  
  
  # Plot of sampled data
  # ===================================
  zeros<-array(0,dim=c(N,1))
  points(DN[1:N],zeros,col='black')
  
  legend("topright",c("Prior","Posterior"),col=c("red","blue"),lty=1)
  
  
}


