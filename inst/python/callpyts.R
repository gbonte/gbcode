rm(list=ls())
library(reticulate)
library(gbcode)
#library(keras)
#py_install("pandas")
#py_install("matplotlib")
#py_install("scikit-learn")
#py_install("lightgbm")
#py_install("pytorch_lightning==1.9.0")
#py_install("darts")
# repl_python()
##reticulate::source_python('~/Dropbox/bontempi_office/Rlang/python/module.py')
#set.seed(0)
set.seed(0)
m=4

N=1500

H=15
TS=array(0,c(N+H,m))

for (j in 1:m){
  for (f in 1)
    TS[,j]=TS[,j]+sin(2*pi*(1:(N+H))/runif(1,50,150))
  TS[,j]=TS[,j]+rnorm(N+H,sd=0.1)
}
#TS[,2]=TS[,1]
TS<-scale(TS)
plot(TS[,1],type="l")
n=1
M<-MakeEmbedded(array(TS[1:N,],c(N,m)),n=numeric(m)+n,numeric(m),hor=numeric(m)+H,w=1:m)


X<-array(M$inp,c(NROW(M$inp),n,m))
Y=M$out #array(M$out,c(NROW(M$out),H,m))


Xts <- NULL
for (j in 1:m)
  Xts<-c(Xts,TS[seq(N,N-n+1,by=-1),j])
Xts<-array(Xts,c(1,n,m))

Yts=TS[(N+1):(N+H),]
Nts=NROW(Xts)
pyX<<-X;   pyXts<<-Xts;   pyY<<-Y;   pyN<<-NROW(X);      pyTS<<-TS[1:N]
pyNts<<-Nts;  pym<<-m;pyn<<-n;pyH<<-H;pynunits<<-48;pynepochs<<-5

plearn<<-"darts_nbeats"
py_run_file("libpy.py") #system.file("python", "libpy.py", package = "gbcode"))


Yhat=py$yhat 
Yhat=array(Yhat,c(H,m))
cat("MSE",plearn,":",mean((Yhat-Yts)^2),"\n")

if (m>1){
  bench="MIMO_rr"
  Yhat2=MmultiplestepAhead(TS[1:N,],n=n,H=H,multi=bench)
  cat("MSE",bench,":",mean((Yhat2-Yts)^2),"\n")
  mts=2
  plot(Yts[,mts],type="l")
  lines(Yhat[,mts],col="red")
  lines(Yhat2[,mts],col="green")
} else {
  Yhat2=multiplestepAhead(TS[1:N,],n=n,H=H)
  print(mean((Yhat2-Yts)^2))
  plot(Yts,type="l")
  lines(Yhat,col="red")
  lines(Yhat2,col="green")
  
}
