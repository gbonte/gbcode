rm(list=ls())
library(reticulate)
#py_install("pandas")
#py_install("matplotlib")
#py_install("scikit-learn")
#py_install("lightgbm")
# repl_python()
##reticulate::source_python('~/Dropbox/bontempi_office/Rlang/python/module.py')
set.seed(0)
N=100
Nts=200
n=5
m=5
class=FALSE
fct<-function(X,sdw=0.1,class=FALSE){
  N=NROW(X)
  n=NCOL(X)
  Y=X[,1]^2*X[,2]+log(abs(X[,n]))+apply(abs(X),1,sum)+rnorm(N,sd=sdw)
  if (class)
    Y=sign(Y-mean(Y))
  return(Y)
}
X=array(rnorm(N*n),c(N,n))
Xts=array(rnorm(Nts*n),c(Nts,n))

Y=NULL
for (j in 1:m)
  Y=cbind(Y,fct(X,class=class))
Yts=NULL
for (j in 1:m)
  Yts=cbind(Yts,fct(Xts,class=class))
pyX<<-X;   pyXts<<-Xts;   pyY<<-Y;   pyN<<-N;   pyn<<-n;   pyNts<<-Nts;  pym<<-m;
plearn<<-"lasso_regr"
py_run_file(system.file("python", "libpy.py", package = "gbcode"))
if (!class){
  print(mean((py$yhat-Yts)^2)/var(c(Yts)))
} else
  print(length(which(Yts!=py$yhat))/Nts)

if (FALSE){
  plearn="lin"
  py_run_file("module2.py")
  print(mean((Yts-py$yhat)^2)/var(Yts))
  
  plearn="pipeab"
  py_run_file("module2.py")
  print(mean((Yts-py$yhat)^2)/var(Yts))
  
  plearn="piperf"
  py_run_file("module2.py")
  print(mean((Yts-py$yhat)^2)/var(Yts))
  
}
