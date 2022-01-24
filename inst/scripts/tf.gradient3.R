library(tensorflow)


Model <- R6::R6Class(
  classname = "Model",
  public = list(
    n=NULL,
    W = NULL,
    b=NULL,
   
    
    initialize = function(n) {
      self$n=n
      self$W <- tf$Variable(array(rnorm(n+1),c(n+1,1)),shape=shape(n+1,1), dtype="float32")
      self$b <- tf$Variable(rnorm(n+1)) 
      ## centroids

    },
    
    predict = function(X) {
      o1=tf$ones(shape(N,n+1))
      
      d=X-tf$matmul(o1,tf$linalg$diag(self$b))
      d2=tf$multiply(d,d)
      d3=tf$exp(-d2)
     
      tf$matmul(d3, self$W)
    }
    
  )
)


loss <- function(y_pred, y_true) {
  tf$reduce_mean(tf$square(y_pred - y_true))
}

n=3

TRUE_W = c(-1,-2,3,4)

N = 100

sdw=0.2

inputs  <- tf$constant(cbind(numeric(N)+1,array(rnorm(N*n),c(N,n))), dtype="float32")
noise   <- tf$random$normal(shape=shape(N,1))
outputs <- as.array(inputs) %*% TRUE_W + sdw*noise



train <- function(model, inputs, outputs, learning_rate) {
  with (tf$GradientTape() %as% t, {
    current_loss = loss(model$predict(inputs), outputs)
  })
  
  d <- t$gradient(current_loss, list(model$W))
  
  with (tf$GradientTape() %as% t, {
    current_loss = loss(model$predict(inputs), outputs)
  })
  
  db <- t$gradient(current_loss, list(model$b))
 
  
  ### Gradient-based step
  model$W$assign_sub(learning_rate * d[[1]])
  model$b$assign_sub(learning_rate * db[[1]])
  current_loss
}


model <- Model$new(n)

Ws <- NULL

losses<-NULL
for (epoch in seq_len(200)) {
  
  Ws<- rbind(Ws, as.numeric(model$W))
 
  current_loss <- train(model, inputs, outputs, learning_rate = 0.1)
  losses=c(losses,as.numeric(current_loss))
  cat(glue::glue("Epoch: {epoch}, Loss: {as.numeric(current_loss)}"), "\n")
} 

par(mfrow=c(1,2))
plot(Ws[,1],ylim=c(min(Ws)-0.1,max(Ws)-0.1),type="l",ylab="Estimations",lwd=2)
abline(h=TRUE_W[1], col="black",lty=2)
for (i in (2:(n+1))){
lines(Ws[,i],col="red",lwd=2)
abline(h=TRUE_W[i], col="red",lty=2)

}

plot(losses,ylim=c(0,5),type="l",ylab="Loss")
