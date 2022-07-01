rm(list=ls())
library("SLBDD")
library("VARshrink")
require(gbcode)
require(MTS)
library(keras)

#FREDMDApril19     Federal Reserve Bank at St Louis.
#CPIEurope200015   m=33 monthly Price Indexes EUUS
# Stockindexes99world World Stock Indexes: daily stock indices of m=99 financial markets
#UMEdata20002018   Quarterly Economic Series of the European Monetary Union (m=57)
# clothing          Cloth sales in China m=25 for 1805 days
#TaiwanAirBox032017 Hourly PM25 Measurements from Air-Box Devices in Taiwan (m=516)
#PElectricity1344 Electricity Prices in New England and USA- weekly (m=1344)
names=c("FREDMDApril19","CPIEurope200015","Stockindexes99world",
        "UMEdata20002018","clothing",
        "TaiwanAirBox032017","PElectricity1344")
frequency=c(24)
nn=3
D = get(names[nn], asNamespace('SLBDD'))
w=which(colnames(D) %in% c("caldt","Date","hour") )
if (length(w)>0)
  D=D[,-w]
print(dim(D))
visualize=TRUE 
season=FALSE
execute=TRUE
namefile="price.Rdata"
methods=c("uni","VAR","VARs","dfm","dfml","multifs")
colors=c("red","green","magenta","cyan","orange","blue")

if (execute){
  set.seed(0)
  n=5
  Nmax<-1000
  mmax<-100
  if (NROW(D)>Nmax)
    D=D[1:Nmax,]
  
 
  if (NCOL(D)>mmax)
    D=D[,sample(1:NCOL(D),mmax)]
  m=NCOL(D)
  H=50
  
  if (season){
    X=NULL
    for (i in 1:m){
      A=ts(D[,i],frequency =frequency[1])
      decomposeA=stats::decompose(A,"additive")
      X=cbind(X,tseries::na.remove(decomposeA$random))
      
    }
    
  } else {
    X=D
  }
  
  
  X=scale(X)
  N=NROW(X)
  Ntr=N-H
  
  Xtr=X[1:Ntr,]
  Xts=X[(Ntr+1):N,]
  
  Xhat1=MmultiplestepAhead(Xtr,n,H,multi=methods[1],uni="stat_naive")
  cat(".")
  Xhat2=MmultiplestepAhead(Xtr,n,H,multi=methods[2])
  cat(".")
  Xhat3=MmultiplestepAhead(Xtr,n,H,multi=methods[3])
  cat(".")
  Xhat4=MmultiplestepAhead(Xtr,n,H,multi=methods[4])
  cat(".")
  Xhat5=MmultiplestepAhead(Xtr,n,H,multi=methods[5],cdfml=2,
                           dfmlmodels=c("lindirect","lazydirect"))
  cat(".")
  Xhat6=MmultiplestepAhead(Xtr,n,H,multi=methods[6],mod="rf")
  cat(".")
  save(file=namefile,list=c("methods","H","X","m","Xts","Xhat1","Xhat2",
                                     "Xhat3","Xhat4","Xhat5","Xhat6"))
} else
  load(namefile)

m=NCOL(X)
e.hat1=apply((Xts-Xhat1)^2,2,mean)
e.hat2=apply((Xts-Xhat2)^2,2,mean)
e.hat3=apply((Xts-Xhat3)^2,2,mean)
e.hat4=apply((Xts-Xhat4)^2,2,mean)
e.hat5=apply((Xts-Xhat5)^2,2,mean)
e.hat6=apply((Xts-Xhat6)^2,2,mean)

cat( methods[1],":",mean(e.hat1)," | ")
cat( methods[2],":",mean(e.hat2)," | ")
cat( methods[3],":",mean(e.hat3)," | ")
cat( methods[4],":",mean(e.hat4)," | ")
cat( methods[5],":",mean(e.hat5)," |")
cat( methods[6],":",mean(e.hat6),"\n")


if (visualize){
  for (ns in 1:m){
    e.hat1=mean((Xts[,ns]-Xhat1[,ns])^2)
    e.hat2=mean((Xts[,ns]-Xhat2[,ns])^2)
    e.hat3=mean((Xts[,ns]-Xhat3[,ns])^2)
    e.hat4=mean((Xts[,ns]-Xhat4[,ns])^2)
    e.hat5=mean((Xts[,ns]-Xhat5[,ns])^2)
    e.hat6=mean((Xts[,ns]-Xhat6[,ns])^2)
    plot(Xts[,ns],type="l",col="black",lwd=3,ylab=paste("ts.",ns),
         main=paste( methods[1],":",round(mean(e.hat1),2)," | ",
                     methods[2],":",round(mean(e.hat2),2)," | ",
                     methods[3],":",round(mean(e.hat3),2)," | ",
                     methods[4],":",round(mean(e.hat4),2)," | ",
                     methods[5],":",round(mean(e.hat5),2)," |",
                     methods[6],":",round(mean(e.hat6),2),"\n"),cex.main=1)
    lines(Xhat1[,ns],col=colors[1])
    lines(Xhat2[,ns],col=colors[2])
    lines(Xhat3[,ns],col=colors[3])
    lines(Xhat4[,ns],col=colors[4])
    lines(Xhat5[,ns],col=colors[5])
    lines(Xhat6[,ns],col=colors[6])
    legend("topleft",
           c("real",methods),
           col=c("black",colors),lty=1,cex=1)
    browser()
  }
}


