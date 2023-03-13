## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

## Automatic differentiation example
## Based on https://tensorflow.rstudio.com/guides/tensorflow/autodiff


library(tensorflow)
library(keras)

g<-function(z){
  1/(1+exp(-z))
}

gp<-function(z){
  exp(-z)/(1+exp(-z))^2
}
x <- tf$constant(rnorm(1))

w11.1 <- tf$Variable(rnorm(1))
w12.1 <- tf$Variable(rnorm(1))

w11.2 <- tf$Variable(rnorm(1))
w21.2 <- tf$Variable(rnorm(1))



with(tf$GradientTape() %as% t, {
  a1.1=w11.1*x
  z1=g(a1.1)
  
  a2.1=w12.1*x
  z2=g(a2.1) #1/(1+exp(-a2.1))
  
  a1.2=w11.2*z1+w21.2*z2
  
  yhat=g(a1.2) #1/(1+exp(-a1.2))
  
})

d <- t$gradient(yhat, list(w11.1,w12.1,w11.2,w21.2))

## 
g11.1=gp(a1.2)*w11.2*gp(a1.1)*x ## dyhat/dw11.1
cat("TF=",as.numeric(d[[1]]), " Analytical=", as.numeric(g11.1),"\n") 

g12.1=gp(a1.2)*w21.2*gp(a2.1)*x ## dyhat/dw12.1
cat("TF=",as.numeric(d[[2]]), " Analytical=",as.numeric(g12.1),"\n") 

g11.2=gp(a1.2)*z1  ## dyhat/dw11.2
cat("TF=",as.numeric(d[[3]]), " Analytical=",as.numeric(g11.2),"\n")

g21.2=gp(a1.2)*z2 ## dyhat/dw21.2
cat("TF=",as.numeric(d[[4]]), " Analytical=", as.numeric(g21.2),"\n")