rm(list=ls())
library(reticulate)
library(gbcode)
library(keras)
#py_install("pandas")
#py_install("matplotlib")
#py_install("scikit-learn")
#py_install("lightgbm")
# repl_python()
##reticulate::source_python('~/Dropbox/bontempi_office/Rlang/python/module.py')
#set.seed(0)
set.seed(0)
m=3

N=100

H=5
TS=array(0,c(N+H,m))

for (j in 1:m){
  for (f in 1:5)
    TS[,j]=TS[,j]+sin(2*pi*(1:(N+H))/runif(1,8,20))
  TS[,j]=TS[,j]+rnorm(N+H,sd=0.1)
  #TS[,j]=seq((100*(j-1)+1),(100*(j-1)+N+H),by=1)
}
#TS[,2]=TS[,1]
##TS<-scale(TS)
plot(TS[,1],type="l")
n=3


Xts <- NULL
for (j in 1:m)
  Xts<-c(Xts,TS[seq(N,N-n+1,by=-1),j])
Xts<-array(Xts,c(1,n,m))

Yts=TS[(N+1):(N+H),]
Nts=NROW(Xts)

nepochs=500
nunits=10
pyTS<<-cbind(TS[1:N,]);   pyXts<<-Xts;    pyY<-array(1:10,c(5,2))   
pyNts<<-Nts;  pym<<-m;pyn<<-n;pyH<<-H;pynepochs<<-nepochs;pynunits<<-nunits;

plearn<<-"transformer_gpt_hyper"
##py_run_file("transformer.py") #system.file("python", "libpy.py", package = "gbcode"))
py_run_file("libpy.py")

Yhat=array(py$yhat,c(H,m))
print(mean((Yhat-Yts)^2))

