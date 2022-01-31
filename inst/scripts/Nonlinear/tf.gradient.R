library(tensorflow)

tfe_enable_eager_execution()


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




TRUE_W = 3.0
TRUE_W2 = 1.0
TRUE_b = 2.0
NUM_EXAMPLES = 1000
sdw=0.2

inputs  <- tf$random$normal(shape=shape(NUM_EXAMPLES))
noise   <- tf$random$normal(shape=shape(NUM_EXAMPLES))
outputs <- inputs * TRUE_W + inputs^2 * TRUE_W2 + TRUE_b + sdw*noise

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
for (epoch in seq_len(200)) {
  
  Ws[epoch] <- as.numeric(model$W)
  Ws2[epoch] <- as.numeric(model$W2)
  bs[epoch] <- as.numeric(model$b)
  
  current_loss <- train(model, inputs, outputs, learning_rate = 0.01)
  losses=c(losses,as.numeric(current_loss))
  cat(glue::glue("Epoch: {epoch}, Loss: {as.numeric(current_loss)}"), "\n")
} 

par(mfrow=c(1,2))
plot(Ws,ylim=c(0,5),type="l",ylab="Estimations",lwd=2)
abline(h=TRUE_W, col="black",lty=2)
lines(Ws2,col="red",lwd=2)
abline(h=TRUE_W2, col="red",lty=2)
lines(bs,col="green",lwd=2)
abline(h=TRUE_b, col="green",lty=2)
legend(x=10,y=5,c("W","W2","b"),lty=1,col=c("black","red","green"))


plot(losses,ylim=c(0,5),type="l",ylab="Loss")
