rm(list=ls())
library(gbcode)
library(keras)

imputeY<-function(Y){
  w=which(is.na(Y))
  N=length(Y)
  if (length(w)==0)
    return(Y)
  
  for (i in 1:length(w)){
    Yinf=Y[max(setdiff(1:w[i],w))]
    Ysup=Y[min(setdiff(w[i]:N,w))]
    Y[w[i]]=mean(Yinf,Ysup)
  }
  return(Y)
  
}
multistepKeras<-function(TS,n,H,arch=1,Nepochs=20,
                         Nstepsperepochs=20,verbose=TRUE){
  Y=TS
  N=length(TS)
  
  mygen<-function(ts,n=1,start=1,end=10,steps=10,H=10){
    
    index<-start
    function(){
      samples=NULL
      targets=NULL
      for (i in  1:steps){
        samples<-rbind(samples,Y[(index):(index+n-1)])
        targets=rbind(targets,Y[(index+n):(index+n-1+H)]) 
        index<<-(index+1)
        if ((index+n+H)>=end)
          index<<-start
      }
      if (any(is.nan(c(targets))))
        browser()
      list(array(samples,c(steps,n,1)), targets)
    }
  }
  train_gen <- mygen(Y,n=n,
                     start=1,
                     end=N,
                     steps=20,
                     H
  )
  
  val_gen <- mygen(Y,n=n,
                   start=10,
                   end=N,
                   steps=20,
                   H
  )
  
  if (arch==1)
    model <- keras_model_sequential() %>% 
    layer_flatten(input_shape = c(n,1)) %>% 
    layer_dense(units = 2*n,
                kernel_regularizer = regularizer_l2(0.01)) %>%
    layer_dense(units = H)
  
  
  if (arch==2)
    model <- keras_model_sequential() %>% 
    layer_gru(units = 2*n, kernel_regularizer = regularizer_l2(0.01),
              input_shape = list(NULL,1)) %>% 
    layer_dense(units = H)
  
  if (arch==3)
    model <- keras_model_sequential() %>% 
    layer_gru(units = 2*n, dropout = 0.2, recurrent_dropout = 0.2,
              input_shape =  list(NULL,1)) %>% 
    layer_dense(units = H)
  
  
  model=compile(model,
                optimizer = optimizer_rmsprop(),
                loss = "mse"
  )
  
  fit_generator(model,
                train_gen,
                steps_per_epoch = Nstepsperepochs,
                epochs = Nepochs,
                validation_data = train_gen,
                validation_steps = 10,
                verbose=verbose
  )
  
  return(model)
}
data(NN5)
ALL<-NULL
ALL2<-NULL
ALL3<-NULL
ALLll<-NULL
ALLll2<-NULL
ALLll3<-NULL
ALLstat<-NULL
ALLstat2<-NULL

# N=2000
# Y=numeric(N)
#F=4
#for (f in 1:F)
#  Y=Y+sin(rnorm(1,sd=0.5)*(1:N))
#Y=Y+rnorm(N,sd=0.2)



for (ts in 2:NCOL(NN5)){
  Y=scale(imputeY(NN5[,ts]))
  
  N=length(Y)
  T0=round(N/2)
  n=10
  
  for ( H in c(2,5,10,20)){
    
    
    model<-multistepKeras(Y[1:T0],n,H)
    model2<-multistepKeras(Y[1:T0],n,H,arch=2)
    model3<-multistepKeras(Y[1:T0],n,H,arch=3)
    
    Nts=N-T0-1-H-n
    
    index=T0+1
    Yhat2=NULL
    Yts=NULL
    Yhat=NULL
    Yhat3=NULL
    Yhatll=NULL
    Yhatll2=NULL
    Yhatll3=NULL
    Yhatstat=NULL
    Yhatstat2=NULL
    
    for (s in 1:Nts){
      p=model%>%predict(array(Y[(index):(index+n-1)],c(1,n,1)))
      Yhat=c(Yhat,p[H])
      p=model2%>%predict(array(Y[(index):(index+n-1)],c(1,n,1)))
      Yhat2=c(Yhat2,p[H])
      p=model3%>%predict(array(Y[(index):(index+n-1)],c(1,n,1)))
      Yhat3=c(Yhat3,p[H])
      Yhatll=c(Yhatll,multiplestepAhead(Y[1:(index+n-1)],n=n,H=H,
                                        method="lazydirect")[H])
      Yhatll2=c(Yhatll2,multiplestepAhead(Y[1:(index+n-1)],n=n,H=H,
                                        method="mimo")[H])
      Yhatll3=c(Yhatll3,multiplestepAhead(Y[1:(index+n-1)],n=n,H=H,
                                          method="lazyiter")[H])
      Yhatstat=c(Yhatstat,multiplestepAhead(Y[1:(index+n-1)],n=n,H=H,
                                          method="stat_theta")[H])
      Yhatstat2=c(Yhatstat2,multiplestepAhead(Y[1:(index+n-1)],n=n,H=H,
                                            method="stat_comb")[H])
      
      Yts=c(Yts,Y[index+n-1+H])
      index=index+1
    }
    
    plot(Yts,type="l")
    lines(Yhat,col="red")
    lines(Yhat2,col="green")
    
    cat("\n ts=",ts,"n=",n , "H=", H ,
        "\n MSE Keras=", (mean((Yts-Yhat)^2)),
        " MSE Keras2=", (mean((Yts-Yhat2)^2)),
        " \n MSE Keras3=", (mean((Yts-Yhat3)^2)),
        "MSE ll=", mean((Yts-Yhatll)^2),
        "MSE ll2=", mean((Yts-Yhatll2)^2),
        "MSE ll3=", mean((Yts-Yhatll3)^2),
        "MSE stat=", mean((Yts-Yhatstat)^2),
        "MSE stat2=", mean((Yts-Yhatstat2)^2),"\n")
  
  
  ALL=c(ALL,mean((Yts-Yhat)^2))
  ALL2=c(ALL2,mean((Yts-Yhat2)^2))
  ALL3=c(ALL3,mean((Yts-Yhat3)^2))
  ALLll=c(ALLll,mean((Yts-Yhatll)^2))
  ALLll2=c(ALLll2,mean((Yts-Yhatll2)^2))
  ALLll3=c(ALLll3,mean((Yts-Yhatll3)^2))
  ALLstat=c(ALLstat,mean((Yts-Yhatstat)^2))
  ALLstat2=c(ALLstat2,mean((Yts-Yhatstat2)^2))
  
  save(file="TSkeras.Rdata",list=c("ALL","ALL2","ALL3",
                                   "ALLll","ALLll2","ALLll3",
                                   "ALLstat","ALLstat2"))
  }
}