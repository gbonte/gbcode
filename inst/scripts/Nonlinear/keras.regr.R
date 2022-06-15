## "INFOF422 Statistical foundations of machine learning" course
## R package gbcode 
## Author: G. Bontempi

rm(list=ls())

library(gbcode)
library(keras)

set.seed(0)
N=1000
n=4

X<-array(rnorm(N*n),c(N,n))
Y=sin(2*pi*apply(X[,c(1,1)],1,mean))+X[,1]+rnorm(N,sd=0.1)

Itr=1:(N/2)
Its=setdiff(1:N,Itr)

Xtr=X[Itr,]
Xts=X[Its,]

Ytr=Y[Itr]
Yts=Y[Its]

num_epochs=100

## regularizer_l2(w) means every coefficient in the weight matrix of the layer
# will  add  w*weight_value  to  the  total  loss  of  the  network.

model <- keras_model_sequential() %>% 
  layer_dense(units = 10, activation = "relu", 
              kernel_regularizer = regularizer_l2(0.01),               
              input_shape = dim(Xtr)[2]) %>%  
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 20, activation = "relu",
              kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%    
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1) 

model %>% 
  compile(    optimizer = "rmsprop",    
              loss = "mse",    metrics = c("mae")  )

history<- model %>% fit(Xtr, Ytr,
              validation_data = list(Xts, Yts),
              epochs = num_epochs, batch_size = 1, verbose = 1)  
Yhats=model %>% predict(Xts)
NMSE=mean((Yts-Yhats)^2)/var(Yts)


Yhatr=model %>% predict(Xtr)
print(mean((Ytr-Yhatr)^2)/var(Ytr))

Yhats2=pred("rf",Xtr,Ytr,Xts,classi=FALSE)
NMSE2=mean((Yts-Yhats2)^2)/var(Yts)

cat("NMSE testset: DNN=", NMSE, " RF=", NMSE2, "\n")