## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

##Use of tensorflow to fit linear parameters by gradient-based iteration

rm(list=ls())
library(tensorflow)

Model <- R6::R6Class(
  classname = "Model",
  public = list(
    W = NULL,
    b = NULL,
    W2=NULL,
    
    initialize = function() {
      self$W <- tf$Variable(0)
      self$b <- tf$Variable(0)
      self$W2 <- tf$Variable(0)
    },
    
    predict = function(x) {
      self$W*x + self$b + self$W2*x^2
    }
    
  )
)


loss <- function(y_pred, y_true) {
  tf$reduce_mean(tf$square(y_pred - y_true))
}

model <- Model$new()


## Parameters
beta1 = 3.0
beta2 = 1.0
beta0 = 2.0
N = 1000
sdw=0.2

## Dataset
inputs  <- tf$random$normal(shape=shape(N))
noise   <- tf$random$normal(shape=shape(N))
outputs <- inputs * beta1+ inputs^2 * beta2 + beta0 + sdw*noise

train <- function(model, inputs, outputs, learning_rate) {
  with (tf$GradientTape() %as% t, {
    current_loss = loss(model$predict(inputs), outputs)
  })
  
  d <- t$gradient(current_loss, list(model$W, model$W2, model$b))
  
  ### Gradient-based step
  model$W$assign_sub(learning_rate * d[[1]])
  model$W2$assign_sub(learning_rate * d[[2]])
  model$b$assign_sub(learning_rate * d[[3]])
  current_loss
}


model <- Model$new()

Ws <- bs <-Ws2 <- c()

losses<-NULL
Nepochs=50
learningrate=0.1
for (epoch in seq_len(Nepochs)) {
  
  Ws[epoch] <- as.numeric(model$W)
  Ws2[epoch] <- as.numeric(model$W2)
  bs[epoch] <- as.numeric(model$b)
  
  current_loss <- train(model, inputs, outputs, learning_rate = learningrate)
  losses=c(losses,as.numeric(current_loss))
  cat(glue::glue("Epoch: {epoch}, Loss: {as.numeric(current_loss)}"), "\n")
} 

par(mfrow=c(1,2))
plot(Ws,ylim=c(0,5),type="l",main=paste("Learning rate=",learningrate),
     ylab="Estimations",lwd=2,xlab="Epochs")
abline(h=beta1, col="black",lty=2)
lines(Ws2,col="red",lwd=2)
abline(h=beta2, col="red",lty=2)
lines(bs,col="green",lwd=2)
abline(h=beta0, col="green",lty=2)
legend(x=10,y=5,c("betahat1","betahat2","betahat0"),
       lty=1,col=c("black","red","green"))


plot(losses,ylim=c(0,5),type="l",ylab="Loss",xlab="Epochs")
