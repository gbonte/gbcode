## Based on https://tensorflow.rstudio.com/tutorials/

library(tensorflow)
library(keras)

w <- tf$Variable(tf$random$normal(c(3L, 2L)), name = 'w')
b <- tf$Variable(tf$zeros(2L, dtype = tf$float32), name = 'b')
x <- as_tensor(1:3, "float32", shape = c(1, 3))

with(tf$GradientTape(persistent = TRUE) %as% tape, {
  y <- tf$matmul(x, w) + b
  loss <- mean(y ^ 2)
})

d<-tape$gradient(y, c(w, b))