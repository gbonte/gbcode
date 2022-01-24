## Based on https://tensorflow.rstudio.com/tutorials/

library(tensorflow)
library(keras)
a <- 3
b <- 5


#tf$config$experimental$list_physical_devices()


## tf automatically converts native R types
add = tf$add(a, b, name = "add")


## from tensor to numeric
print(as.numeric(add^2+sqrt(add)))

mul = tf$multiply(a, b)
div <- tf$divide(a, b)

## TensorFlow operations automatically convert R arrays to Tensors.
A=tf$constant(array(1:100,c(10,10)))

A$shape
A$dtype

a <- tf$constant(c(1, 2), shape = c(1L, 2L), name = "a")
b <- tf$constant(c(3, 4), shape = c(2L, 1L), name = "b")
c <- tf$matmul(a, b, name = "mat_mul")

x <- tf$random$normal(shape(3, 3))

## which device is x placed on
x$device


m=4
N=10
n=8
layer <- layer_dense(units = m, input_shape = shape(NULL, 1))
## units is the number of outputs

x=tf$ones(shape(N, n))

# The number of input dimensions is often unnecessary, as it can be inferred
# the first time the layer is used

y=layer(x)
Y=as.array(y)
dim(Y)


x=tf$ones(shape(10, 8, 5, 30))
layer2 <- layer_conv_2d(filters = 10,kernel_size=c(1,1))
y=layer2(x)
tf$shape(y)
dim(as.array(y))



x1=3
x2=1
x3=3
X1 <- tf$constant(x1)
X2<-tf$constant(x2)

with(tf$GradientTape() %as% t, {
  t$watch(X1)
  t$watch(X2)
  Y <- tf$math$log(2*(X1^2+X2))
  Z <- tf$math$log(Y) # exp(x1+x2)
})

Z
dZ_dX <- t$gradient(Z, X1)
dZ_dX
2*x1/((x1^2+x2)*log(2*(x1^2+x2))) ## https://tinyurl.com/h6z4k6bf





