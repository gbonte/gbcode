rm(list=ls())
library(tensorflow)

n=3
p=n+1
nrbf=10
rate=0.05
lin=TRUE
nepochs=505
TRUEW = rnorm(p)
N = 100
sdw=0.25

set.seed(0)
X=cbind(numeric(2*N)+1,array(rnorm(2*N*n),c(2*N,n)))
Y=scale(sin(2*pi*(X%*%TRUEW))+rnorm(N,sd=sdw))
Ytr=Y[1:N]
Xtr=X[1:N,]
inputs  <- tf$constant(Xtr, dtype="float32")
outputs <- tf$constant(Ytr)
outputs<-tf$reshape(outputs,shape=shape(N,1)) 
inpuTS  <- tf$constant(X[(N+1):(2*N),], dtype="float32")
Yts=Y[(N+1):(2*N)]


SC = 1
rho<-function( distances ){
  tf$exp( -SC * distances * distances )
}

norm<-function( x,N,r ){
  x #/ tf$matmul(tf$reshape(tf$reduce_sum( x,axis=1L),shape=shape(N,1)),tf$ones(shape=shape(1,r)))
}


LModel <- R6::R6Class(
  classname = "LModel",
  public = list(
    n=NULL,
    W = NULL,
    
    initialize = function(p) {
      self$n=n
      self$W <- tf$Variable(array(rnorm(p),c(p,1)),shape=shape(p,1), dtype="float32")
      
    },
    
    predict = function(X) {
      
      tf$matmul(X, self$W)
    }
    
  )
)



RBFModel <- R6::R6Class(
  classname = "RBFModel",
  public = list(
    A1=NULL,
    A2 = NULL,
    c=NULL,
    nrbf=NULL,
    
    initialize = function(n,r) {
      self$nrbf = r
      self$A1 = tf$Variable(tf$random$normal(shape=shape(n, nrbf)))  # matrix A1: input -> first layer nodes
      self$A2 = tf$Variable(tf$random$normal(shape=shape(nrbf, 1) )) # first_layer nodes -> sum node
      self$c = tf$Variable(tf$random$normal(shape=shape(nrbf)))  # centroids
      
    },
    
    predict = function(X) {
      N=NROW(as.array(X))
      inputs_with_weights = tf$matmul( X, self$A1 )
      
      distances = inputs_with_weights - self$c
      first_output = norm( rho( distances),N,self$nrbf  ) # tf_gaussian_function(distances) # 
      output = tf$matmul( first_output, self$A2 )
      ##output = tf$matmul( inputs_with_weights, self$A2 )
      output
    }
    
  )
)


loss <- function(y_pred, y_true) {
  tf$reduce_mean(tf$square(y_pred - y_true))
}


RBFtrain <- function(model, inputs, outputs, learning_rate) {
  with (tf$GradientTape() %as% t, {
    current_loss = loss(model$predict(inputs), outputs)
  })
  
  d <- t$gradient(current_loss, list(model$c))
  
    model$c$assign_sub(0.1*learning_rate * d[[1]])
  
  with (tf$GradientTape() %as% t, {
    current_loss = loss(model$predict(inputs), outputs)
  })
  
  d1 <- t$gradient(current_loss, list(model$A1))
  
  model$A1$assign_sub(learning_rate * d1[[1]])
  
  with (tf$GradientTape() %as% t, {
    current_loss = loss(model$predict(inputs), outputs)
  })
  
  d2 <- t$gradient(current_loss, list(model$A2))
  model$A2$assign_sub(learning_rate * d2[[1]])
  
  current_loss
}

Ltrain <- function(model, inputs, outputs, learning_rate) {
  with (tf$GradientTape() %as% t, {
    current_loss = loss(model$predict(inputs), outputs)
  })
  
  d <- t$gradient(current_loss, list(model$W))
  
  model$W$assign_sub(learning_rate * d[[1]])
  
  
  current_loss
}

if (lin){
  model <- LModel$new(p) #,nrbf)
}else{
  model <- RBFModel$new(p,nrbf)
}
Ws <- NULL

losses<-NULL
for (epoch in seq_len(nepochs)) {
  
  #Ws<- rbind(Ws, as.numeric(model$W))
  if (lin)
    current_loss <- Ltrain(model, inputs, outputs, learning_rate = rate)
  else
    current_loss <- RBFtrain(model, inputs, outputs, learning_rate = rate)
  losses=c(losses,as.numeric(current_loss))
  cat(glue::glue("Epoch: {epoch}, Loss: {as.numeric(current_loss)}"), "\n")
} 


yhatTS=model$predict(inpuTS)

print(mean((as.array(yhatTS)-Yts)^2))

yhatTR=model$predict(inputs)

print(mean((c(as.array(yhatTR))-c(Ytr))^2))

