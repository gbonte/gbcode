# Core Tidyverse
## From https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/
rm(list=ls())
library(gbcode)
library(tidyverse)
#library(glue)


# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
#library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
#library(yardstick) 

# Modeling
library(keras)


sun_spots <- datasets::sunspot.month %>%
  tk_tbl() %>%
  mutate(index = as_date(index)) %>%
  as_tbl_time(index = index)

H<-40

periods_train <- 12 * 70
periods_test  <- H
skip_span     <- 12

rolling_origin_resamples <- rolling_origin(
  sun_spots,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)


lstmpred<-function(TS,H,nunits=10,epochs=20){
  
  m=NCOL(TS)
  N=NROW(TS)
  if (m==1){
    x_train_arr <- array(TS[1:(N-H)], c(N-H,1,1))
    y_train_arr <- array(TS[(H+1):(N)], c(N-H,1))
    x_test_arr <- array(TS[(H+1):(N)], c(N-H,1,1))
  } else {
    x_train_arr <- array(TS[1:(N-H),], c(N-H,1,1))
    y_train_arr <- array(TS[(H+1):(N),],c(N-H,1)) #lag_train_tbl$value
    x_test_arr <- array(TS[(H+1):(N),], c(N-H,1,1))
  }
  
  batch_size=1
  
  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(units            = nunits, 
               input_shape      = c(m,1), 
               batch_size       = batch_size,
               return_sequences = TRUE, 
               stateful         = TRUE) %>% 
    layer_lstm(units            = nunits, 
               return_sequences = FALSE, 
               stateful         = TRUE) %>% 
    layer_dense(units = m)
  
  model %>% 
    compile(loss = 'mae', optimizer = 'adam')
  
  # 5.1.7 Fitting LSTM
  for (i in 1:epochs) {
    model %>% fit(x          = x_train_arr, 
                  y          = y_train_arr, 
                  batch_size = batch_size,
                  epochs     = 1, 
                  verbose    = -1, 
                  shuffle    = FALSE)
    
    model %>% reset_states()
    #cat("Epoch: ", i)
    
  }
  
  # 5.1.8 Predict and Return Tidy Data
  # Make Predictions
  pred_out <- model %>% 
    predict(x_test_arr, batch_size = batch_size)
 
  N2=NROW(pred_out)
  
  
  return(pred_out[(N2-H+1):N2,])
  
  
}

predict_keras_lstm2 <- function(TS, H,epochs = 300, ...) {
  # 5.1.4 LSTM Plan
  
  batch_size   <- 10
  tsteps       <- 1
  epochs       <- epochs
  
  N=length(TS)
  x_train_vec <- TS[1:(N-H)]
  
  x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
  ##
  
  y_train_vec <- TS[(H+1):(N)] #lag_train_tbl$value
  y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
  ##
  
 
  # 5.1.6 LSTM Model
  model <- keras_model_sequential()
  
  model %>%
    layer_lstm(units            = 50, 
               input_shape      = c(tsteps, 1), 
               batch_size       = batch_size,
               return_sequences = TRUE, 
               stateful         = TRUE) %>% 
    layer_lstm(units            = 50, 
               return_sequences = FALSE, 
               stateful         = TRUE) %>% 
    layer_dense(units = 1)
  
  model %>% 
    compile(loss = 'mae', optimizer = 'adam')
  
  # 5.1.7 Fitting LSTM
  for (i in 1:epochs) {
    model %>% fit(x          = x_train_arr, 
                  y          = y_train_arr, 
                  batch_size = batch_size,
                  epochs     = 1, 
                  verbose    = -1, 
                  shuffle    = FALSE)
    
    model %>% reset_states()
    #cat("Epoch: ", i)
    
  }
  x_test_vec <- TS[(H+1):(N)]
  x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
  
  # 5.1.8 Predict and Return Tidy Data
  # Make Predictions
  pred_out <- model %>% 
    predict(x_test_arr, batch_size = batch_size) %>%
    .[,1] 
  
  N2=length(pred_out)
  
  return(pred_out[(N2-H+1):N2])
  
  
}
method1="lstm"
method2="mimo.acf"
method3="lazydirect"
method4="rnn"
colors=c("red","green","magenta","cyan","orange","yellow")
L=length(rolling_origin_resamples$splits)
print(L)

aLSTM2=NULL
aLL1=NULL
aLL2=NULL
aLL3=NULL
aLL4=NULL
for (S in round(L/2):L){
  split    <- rolling_origin_resamples$splits[[S]]
  df_trn <- training(split)
  df_tst <- testing(split)
  TStrain=df_trn$value
  TStest=df_tst$value
  H=length(TStest)
  mu=mean(TStrain)
  stdTS=sd(TStrain)
  nTStrain=(TStrain-mu)/stdTS
  
#  predkeras=predict_keras_lstm2(nTStrain,H, epochs = 50)*stdTS+mu
 # predkeras=lstmpred(nTStrain,H=H, nunits=50, epochs = 50)*stdTS+mu
  
  Yhat1=multiplestepAhead(nTStrain,120,
                         H=H,method = method1,nunits=50, 
                         epochs = 500)*stdTS+mu
 
  Yhat2=multiplestepAhead(nTStrain,120,
                         H=H,method = method2,Kmin=3,C=3)*stdTS+mu
  Yhat3=multiplestepAhead(nTStrain,120,
                         H=H,method = method3,Kmin=3,C=3)*stdTS+mu
  
  Yhat4=multiplestepAhead(nTStrain,120,
                          H=H,method = method1,nunits=50, 
                          epochs = 500)*stdTS+mu
  
  
  aLL1=c(aLL1,mean(abs(TStest-Yhat1)))
  aLL2=c(aLL2,mean(abs(TStest-Yhat2)))
  aLL3=c(aLL3,mean(abs(TStest-Yhat3)))
  aLL4=c(aLL4,mean(abs(TStest-Yhat4)))
  cat("Split=",S, "length(TS)=", length(TStrain), 
      " H=",H,"|", method1, "=",mean(aLL1), 
      method2, "=",mean(aLL2),
      method3, "=",mean(aLL3), method4, "=",mean(aLL4) ,"\n")
  plot(c(TStrain,TStest))
  points(c(NA*TStrain,TStest),lwd=2)
  lines(c(NA*TStrain,Yhat1),col=colors[1])
  lines(c(NA*TStrain,Yhat2),col=colors[2])
  lines(c(NA*TStrain,Yhat3),col=colors[3])
  lines(c(NA*TStrain,Yhat3),col=colors[4])
  legend("topleft",c(method1,method2,method3,method4),
         col=colors,lty=1,cex=0.5)
  Sys.sleep(1)
}