rm(list=ls())
library(reticulate)
#py_install("pandas")
#py_install("matplotlib")
#py_install("scikit-learn")
#py_install("lightgbm")
# repl_python()
##reticulate::source_python('~/Dropbox/bontempi_office/Rlang/python/module.py')
N=1000
Nts=1000
n=50 
class=TRUE
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
Y=fct(X,class=class)
Yts=fct(Xts,class=class)


plearn="rf_class"
py_run_file("libpy.py")
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
