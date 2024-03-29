## "Statistical foundations of machine learning" software
## R package gbcode 
## Author: G. Bontempi


rm(list=ls())
N=50000
Sigma=rbind(c(1,0.5,0.3),
        c(0.5, 1,0.3),
        c(0.3, 0.3,1)) # %Correlation matrix
A=chol(Sigma) # %Cholesky decomposition 

D=array(rnorm(N*3),c(N,3)) # %Random data in three columns each for X,Y and Z
Dc=D%*%A; #%Correlated matrix Rc=[X Y Z]

# var(U*z)= U^T var(z) U =U^T U = C  
cat("\n ---\n Target Sigma=\n")
print(Sigma)
cat("\n ---\n Estimated Sigma=\n")
print(cov(Dc))