rm(list=ls())
library(gbcode)


data(NN5)

nseries<-NCOL(NN5)
H=50
N=NROW(NN5)
NMSE=NULL
NMSE2=NULL
NMSE3=NULL
NMSE4=NULL
NMSE5=NULL
NMSE6=NULL

method1="lazydirect"
method2="liniter"
method3="lazyiter"
method4="stat_holt"
method5="rnn"
method6="mimo.comb"
colors=c("red","green","magenta","cyan","orange","blue")
visualize=TRUE
deseason=TRUE
n=24
maxfs=5
for (i in 2:nseries){
  TS=tseries::na.remove(NN5[,i])
  if (deseason){
    A=tseries::na.remove(ts(TS,frequency =7))
    decomposeA=stats::decompose(A,"additive")
    TS=tseries::na.remove(scale(decomposeA$random))
  }
  N=length(TS)
  for (Ntr in round(seq (500,N-H,length.out=3))){
    
    TStr=TS[1:Ntr]
    TSts=TS[(Ntr+1):(Ntr+H)]
    Yn=multiplestepAhead(TStr,n=n, H=H,method="stat_naive")
    
    Y.cont=multiplestepAhead(TStr,n=n, H=H,method=method1)
    NMSE=c(NMSE,mean((TSts-Y.cont)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont2=multiplestepAhead(TStr,n=n, H=H,method=method2)
    NMSE2=c(NMSE2,mean((TSts-Y.cont2)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont3=multiplestepAhead(TStr,n=n, H=H,method=method3)
    NMSE3=c(NMSE3,mean((TSts-Y.cont3)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont4=multiplestepAhead(TStr,n=n, H=H,method=method4)
    NMSE4=c(NMSE4,mean((TSts-Y.cont4)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont5=multiplestepAhead(TStr,n=n, H=H,method=method5)
    NMSE5=c(NMSE5,mean((TSts-Y.cont5)^2)/(mean((TSts-Yn)^2)))
    
    Y.cont6=multiplestepAhead(TStr,n=n, H=H,method=method6)
   
    NMSE6=c(NMSE6,mean((TSts-Y.cont6)^2)/(mean((TSts-Yn)^2)))
    
    ## Forecasting error is normalised wrt error of the naive method
    
    cat("series",i,"/", nseries,  "\n", method1, " NMSE=",mean(NMSE),
        method2," NMSE2=", mean(NMSE2), ":",
        method3, " NMSE3=", mean(NMSE3),": \n",
        method4, " NMSE4=", mean(NMSE4),":",
        method5, " NMSE=", mean(NMSE5),":",
        method6, " NMSE=", mean(NMSE6),"\n")
    if (visualize){
      plot(TSts,type="l")
      lines(Y.cont,col=colors[1])
      lines(Y.cont2,col=colors[2])
      lines(Y.cont3,col=colors[3])
      lines(Y.cont4,col=colors[4],lwd=2)
      lines(Y.cont5,col=colors[5])
      lines(Y.cont6,col=colors[6])
      legend("topleft",
             c(method1,method2,method3,method4,method5,method6),
             col=colors,lty=1)
      browser()
    }
  }
}