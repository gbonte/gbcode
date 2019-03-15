
rm(list=ls())
library(abind)
library(gbcode)
set.seed(0)
Wx=100 ## width arena
Wy=100

## initialization probability mass
P=array(1/(Wx*Wy),c(Wx,Wy))
Sdb<-NULL
Adb<-NULL
Xdb<-NULL
S=numeric(4) # Sensors: east, south,west, north
x=Wx/2
y=Wy/2

zero=1e-10
sdS=1
sdX=1

visualize=FALSE
simulate=TRUE
estimate=FALSE
makedata=TRUE
T=20000

if (simulate){
  for (t in 1:T){
    
    # control action
    ux<-rnorm(1,sd=2) #sample(c(-2,-1,0,1,2),1)
    uy<-rnorm(1,sd=2)  #sample(c(-2,-1,0,1, 2),1)
    
    
    # update position robot
    x=max(1,min(x+ux,Wx))
    y=max(1,min(y+uy,Wy))
    
    if (estimate){
      ## Bayes state update after control action
      for (i in 1:Wx){
        for (j in 1:Wy){
          prev.i=round(max(1,min(i-ux,Wx)))
          prev.j=round(max(1,min(j-uy,Wy)))
          P[i,j]=P[prev.i,prev.j]
          
        }
      }
      P=P/sum(P)
    }
    
    ## robot slippery
    x=(max(1,min(x+rnorm(1,sd=sdX),Wx)))
    y=(max(1,min(y+rnorm(1,sd=sdX),Wy)))
    
    
    ### sensor data collection
    S=pmax(pmin(c(Wx-x,y-1,x-1,Wy-y)+rnorm(4,sd=sdS),Wx),0)
    Sdb<-rbind(Sdb,S)
    Adb<-rbind(Adb,c(ux,uy))
    Xdb<-rbind(Xdb,c(x,y))
    
    if (estimate){
      ## Bayes state update after sensing 
      
      for (i in 1:Wx){
        for (j in 1:Wy){
          Pt=log(P[i,j])
          Lik=dnorm(S[1],mean=Wx-i,sd=sdS,log=TRUE)+dnorm(S[2],mean=j-1,sd=sdS,log=TRUE)+
            dnorm(S[3],mean=i-1,sd=sdS,log=TRUE)+dnorm(S[4],mean=Wy-j,sd=sdS,log=TRUE)
          
          P[i,j]=max(zero,exp(Pt+Lik))
          
        }
      }
      P=P/sum(P)
    }
    
    if (visualize){
      ## visualization of robot position vs state density estimation
      image(1:Wx,1:Wy,P, col = grey(seq(0, 1, length = 256)),
            main="Bayes robot state estimation",
            xlab="",ylab="")
      points(x,y,col="red",lwd=5)
      Sys.sleep(0.1)
    }
    cat(".")
  }
  
  save(file="robot.Rdata",list=c("Adb","Sdb","Xdb"))
  print("saved data")
}
if (makedata){
  
  load("robot.Rdata")
  Adb=scale(Adb)
  Sdb<-scale(Sdb)
  input_train=NULL
  y_train=NULL
  x_train<-NULL
  nbatches=10000
  duration=100
  for (r in sample(2:(NROW(Adb)-duration),nbatches)){
    Itr<-seq(r,r+duration,by=1)
    Imatrix<-cbind(Adb[Itr,],Sdb[Itr-1,])
    it<-array(Imatrix,c(1,NROW(Imatrix),NCOL(Imatrix)))
    input_train<-abind(along=1,input_train,it)
    
    Omatrix<-Sdb[Itr,]
    ot<-array(Omatrix,c(1,NROW(Omatrix),NCOL(Omatrix)))
    y_train<-abind(along=1,y_train,ot)
    
    Xmatrix<-Xdb[Itr,]
    xt<-array(Xmatrix,c(1,NROW(Xmatrix),NCOL(Xmatrix)))
    x_train<-abind(along=1,x_train,xt)
    
    if (r%%100==0)
      cat("*")
  }
  
  save(file="robot.Rdata",list=c("Adb","Sdb","Xdb","input_train","y_train","x_train"))
  print("saved data")
}
library(keras)

load("robot.Rdata")

print(dim(input_train))
print(dim(y_train))

model <- keras_model_sequential() %>%
  layer_simple_rnn(units = 132,
                   input_shape =c(dim(input_train)[2],dim(input_train)[3]),name="rnn",
                   return_sequences = TRUE) %>%
  # layer_simple_rnn(units = 2,return_sequences = TRUE,name="rnn2")%>%
  #layer_simple_rnn(units = 4,return_sequences = TRUE,name="rnn3") %>%
  layer_dense(units = 100,activation="linear", kernel_initializer='normal')%>%
  layer_dense(units = 2,name="state",activation="linear", kernel_initializer='normal')%>%
  layer_dense(units = 4,activation="linear")



model %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

history <- model %>% fit(
  input_train, y_train,
  epochs = 50,
  batch_size = 128,
  validation_split = 0.2
)

intermediate_layer_model <- keras_model(inputs = model$input,
                                        outputs = get_layer(model,"state" )$output)

int_output = intermediate_layer_model%>%predict(input_train)

output = model%>%predict(input_train)

X=c(x_train[,,1])
Xhat=c(int_output[,,1])



Y=c(x_train[,,2])
Yhat=c(int_output[,,2])




print(cor(c(x_train[,,1]),c(int_output[,,1])))
print(cor(c(x_train[,,2]),c(int_output[,,2])))
TT=dim(y_train)[2]
for (nb in 1:dim(y_train)[1]){
  
  print(mean(abs(y_train[nb,1:TT,]-output[nb,1:TT,])))
  
  par(mfrow=c(1,3))
  
  plot(y_train[nb,1:TT,1])
  lines(output[nb,1:TT,1])
  
  xhat=pred("lin",Xhat,X,int_output[nb,1:TT,1],class=FALSE) 
  yhat=pred("lin",Yhat,Y,int_output[nb,1:TT,2],class=FALSE) 
  #plot(int_output[nb,1:TT,1],int_output[nb,1:TT,2],type="l",   xlim=c(min(int_output[,,1]),max(int_output[,,1])),ylim=c(min(int_output[,,2]),max(int_output[,,2])))
  
  plot(xhat,yhat,type="l",xlim=c(1,Wx),ylim=c(1,Wy))
  
  plot(x_train[nb,1:TT,1],x_train[nb,1:TT,2],col="red",type="l",xlim=c(1,Wx),ylim=c(1,Wy))
  readline(prompt = "")
}